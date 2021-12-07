import * as echarts from 'echarts'
import {
  onMounted
} from 'vue'

export default function useBaseLineEcharts({
  elementId,
  title,
  xAxisData
}) {
  onMounted(() => {
    var chartDom = document.getElementById(elementId)
    var myChart = echarts.init(chartDom)
    var option

    option = {
      title: {
        text: title,
        left: 'center'
      },
      grid: {
        left: '8%',
        right: '5%',
        bottom: '15%'
      },
      tooltip: {
        trigger: 'axis',
        axisPointer: {
          type: 'cross'
        }
      },
      toolbox: {
        show: true,
        feature: {
          dataZoom: {
            yAxisIndex: 'none'
          },
          dataView: {
            readOnly: false
          },
          magicType: {
            type: ['line', 'bar']
          },
          saveAsImage: {}
        }
      },
      xAxis: {
        type: 'category',
        data: xAxisData
      },
      yAxis: {
        type: 'value'
      },
      series: [{
        data: [150, 230, 224, 218, 135, 147, 260],
        type: 'line'
      }]
    }

    option && myChart.setOption(option)
  })
}
