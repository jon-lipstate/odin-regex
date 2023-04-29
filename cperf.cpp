#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <x86intrin.h>

typedef enum { MATCH_EPSILON, MATCH_RANGE } MatchType;

typedef struct {
  MatchType type;
  int min;
  int max;
  int invert;
} Match;

typedef struct {
  int to;
  Match match;
} Transition;

typedef struct {
  Transition **transitions;
  int transition_count;
  int start;
  int end;
} NFA;
int match_transition(Match match, char r) {
  if (match.type == MATCH_EPSILON) {
    return 1; // !!!!!!! Epsilon transition cannot be matched by input??
  } else if (match.type == MATCH_RANGE) {
    int in_range = (r >= match.min) && (r <= match.max);
    return match.invert ? !in_range : in_range;
  }
  return 0;
}

void update_active_states(NFA *nfa, uint64_t *active_states, int state) {
  static int stack[6];
  static int stack_size = 0;

  stack[stack_size++] = state;

  while (stack_size > 0) {
    state = stack[--stack_size];

    if ((*active_states >> state) & 1)
      continue;
    *active_states |= (1ULL << state);

    for (int i = 0; i < 2; i++) {
      Transition t = nfa->transitions[state][i];
      if (t.to == -1)
        break;
      if ((*active_states >> t.to) & 1)
        continue;
      if (match_transition(t.match, 0)) {
        *active_states |= (1ULL << t.to);
        stack[stack_size++] = t.to;
      }
    }
  }
}

bool match(NFA *nfa, const char *input) {
  uint64_t states_a = 0;
  uint64_t states_b = 0;

  uint64_t *current_states = &states_a;
  uint64_t *next_states = &states_b;

  size_t slen = strlen(input);
  update_active_states(nfa, current_states, nfa->start);

  for (size_t i = 0; i < slen; i++) {
    char r = input[i];
    states_b = 0;
    for (int state = 0; state < 6; state++) {
      if (!((*current_states >> state) & 1))
        continue;
      for (int j = 0; j < 2; j++) {
        Transition t = nfa->transitions[state][j];
        if (t.to == -1)
          break;
        if ((*next_states >> t.to) & 1)
          continue;
        if (match_transition(t.match, r)) {
          update_active_states(nfa, next_states, t.to);
          *next_states |= (1ULL << t.to);
        }
      }
    }
    // Swap pointers
    uint64_t *temp = current_states;
    current_states = next_states;
    next_states = temp;
  }

  return (*current_states >> nfa->end) & 1;
}

NFA create_nfa() {
  NFA nfa;

  nfa.transitions = (Transition **)malloc(6 * sizeof(Transition *));
  nfa.transition_count = 6;
  nfa.start = 0;
  nfa.end = 5;

  Transition t0[] = {
      {.to = 1,
       .match = {.type = MATCH_RANGE, .min = '0', .max = '9', .invert = 0}}};
  nfa.transitions[0] = (Transition *)malloc(sizeof(t0));
  memcpy(nfa.transitions[0], t0, sizeof(t0));

  Transition t1[] = {
      {.to = 0, .match = {.type = MATCH_EPSILON}},
      {.to = 2,
       .match = {.type = MATCH_RANGE, .min = '-', .max = '-', .invert = 0}}};
  nfa.transitions[1] = (Transition *)malloc(sizeof(t1));
  memcpy(nfa.transitions[1], t1, sizeof(t1));

  Transition t2[] = {
      {.to = 3,
       .match = {.type = MATCH_RANGE, .min = '0', .max = '9', .invert = 0}}};
  nfa.transitions[2] = (Transition *)malloc(sizeof(t2));
  memcpy(nfa.transitions[2], t2, sizeof(t2));

  Transition t3[] = {
      {.to = 2, .match = {.type = MATCH_EPSILON}},
      {.to = 4,
       .match = {.type = MATCH_RANGE, .min = '-', .max = '-', .invert = 0}}};
  nfa.transitions[3] = (Transition *)malloc(sizeof(t3));
  memcpy(nfa.transitions[3], t3, sizeof(t3));

  Transition t4[] = {
      {.to = 5,
       .match = {.type = MATCH_RANGE, .min = '0', .max = '9', .invert = 0}}};
  nfa.transitions[4] = (Transition *)malloc(sizeof(t4));
  memcpy(nfa.transitions[4], t4, sizeof(t4));

  Transition t5[] = {{.to = 4, .match = {.type = MATCH_EPSILON}}};
  nfa.transitions[5] = (Transition *)malloc(sizeof(t5));
  memcpy(nfa.transitions[5], t5, sizeof(t5));

  return nfa;
}

int main() {
  // TODO: This code was whipped up and is probably buggy, its not matching
  // correct for sure Verify NFA more closely, double check all procs.
  // gcc -o3 -o cperf cperf.cpp
  // 6k-20k cycles as written, parity with odin (i think?)
  NFA nfa = create_nfa();

  const char *regex = "([0-9]+)-([0-9]+)-([0-9]+)";
  const char *str = "650-253-0001";

  uint64_t start_tsc = __rdtsc();
  bool result = match(&nfa, str);
  uint64_t clocks = __rdtsc() - start_tsc;
  double desktop_freq = 3493399053.0;
  printf("clocks: %llu cy, ns: %.0f\n", clocks,
         double(clocks) / desktop_freq * 1000000000);
  printf("match: %s\n", result ? "true" : "false");

  return 0;
}
