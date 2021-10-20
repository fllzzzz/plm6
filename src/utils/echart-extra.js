
/**
 * echart 数据视图表格option
 * @param {*} opt
 */
export function echartDateDataViewOptionToContent(opt) {
  var axisData = opt.xAxis[0].data
  var series = opt.series
  var table = `
  <table class="pure-table" style="width:100%;user-select: text;">
    <thead>
      <tr>
        <th>时间</th>`
  for (let i = 0, l = series.length; i < l; i++) {
    table += `
        <th>${series[i].name}</th>`
  }
  table += ` </tr></thead> <tbody>`
  for (let i = 0, l = axisData.length; i < l; i++) {
    table += `
    <tr>
      <td>${axisData[i]}</td>`
    for (let j = 0, sl = series.length; j < sl; j++) {
      table += `<td>${series[j].data[i]}</td>`
    }
    table += `</tr>`
  }
  table += `</tbody></table>`
  return table
}
