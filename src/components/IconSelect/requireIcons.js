
const req = import.meta.globEager('../../icons/svg/**/*.svg')
const requireAll = []
for (const key in req) {
  requireAll.push(key)
}
const icons = requireAll.map(i => {
  const dir = i.replace(/(.*\/)*([^.]+).*/ig, '$1').split('svg/')[1].replace(/\//g, '')
  const svg = i.replace(/(.*\/)*([^.]+).*/ig, '$2')
  return dir ? dir + '-' + svg : svg
})
export default icons
