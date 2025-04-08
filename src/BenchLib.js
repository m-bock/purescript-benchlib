export const performanceNow = () => {
  if (typeof performance !== 'undefined' && typeof performance.now === 'function') {
    return performance.now();
  } else {
    return Date.now();
  }
}