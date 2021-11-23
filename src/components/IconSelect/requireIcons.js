
const req = import.meta.globEager('../../icons/svg/**/*.svg')
const requireAll = []
for (const key in req) {
  requireAll.push(key)
}
const icons = requireAll.map(i => {
  return i.replace(/(.*\/)*([^.]+).*/ig, '$2')
})
export default icons
