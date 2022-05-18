import * as echarts from 'echarts'
import { onMounted } from 'vue'

export default function useChart({ elementId, fetchHook, initOption = {}}) {
  let myChart = null

  onMounted(async () => {
    // 解决高度自适应问题
    setTimeout(() => {
      initChart(elementId)
      if (typeof fetchHook === 'function') {
        fetchHook(myChart)
      }
    })
  })

  function getMyChart() {
    return myChart
  }

  function initChart(elementId) {
    var chartDom = document.getElementById(elementId)
    myChart = echarts.init(chartDom, null, { locale: 'ZH' })

    const labelOption = {
      show: false,
      position: 'insideBottom',
      distance: 15,
      align: 'center',
      verticalAlign: 'middle',
      rotate: 90,
      formatter: '{c}',
      color: '#fff',
      fontSize: 14,
      rich: {
        name: {}
      },
      ...initOption?.labelOption
    }
    const option = {
      title: {
        ...initOption?.title
      },
      tooltip: {
        show: true,
        trigger: 'axis',
        axisPointer: {
          type: 'shadow'
        },
        ...initOption?.tooltip
      },
      grid: {
        right: '1%', left: '1%', bottom: 0, containLabel: true,
        ...initOption?.grid
      },
      toolbox: {
        show: false,
        orient: 'horizontal',
        left: 'right',
        top: 'top',
        feature: {
          mark: {
            show: true
          },
          dataView: {
            show: false
          },
          magicType: {
            show: true,
            type: ['line', 'bar']
          },
          restore: {
            show: false
          },
          saveAsImage: {
            show: true
          },
          ...initOption?.toolboxFeature
        },
        ...initOption?.toolbox
      },
      // color: [...initOption?.color],
      legend: {
        ...initOption?.legend
      },
      xAxis: [{
        type: 'category',
        axisTick: {
          show: false
        },
        ...initOption?.xAxis
      }],
      yAxis: [{
        type: 'value',
        ...initOption?.yAxis
      }],
      series: initOption?.series?.reduce((arr, pos) => {
        arr.push({
          label: labelOption,
          emphasis: {
            focus: 'series'
          },
          ...pos
        })
        return arr
      }, []),
      // 自定义 可覆盖前面默认设置
      ...initOption.custom
    }

    console.log(option, 'option')
    option && myChart.setOption(option)
  }

  return {
    getMyChart
  }
}
