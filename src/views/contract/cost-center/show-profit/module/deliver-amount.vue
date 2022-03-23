<template>
  <div>
    <el-table
      v-loading="loading"
      :border="$TBS.BORDER"
      :stripe="$TBS.STRIPE"
      :data="list"
      :max-height="$_height"
      show-summary
      :summary-method="getSummaries"
      empty-text="暂无数据"
      style="width: 100%;"
    >
      <el-table-column v-if="$TBS.INDEX" :label="$TBS.INDEX_LABEL ? '序号': ''" type="index" align="center" width="60" />
      <el-table-column key="monomerName" prop="monomerName" label="单体" />
      <el-table-column key="name" prop="name" label="名称" min-width="120px" />
      <el-table-column key="serialNumber" prop="serialNumber" label="编号" />
      <el-table-column key="specification" prop="specification" label="规格" min-width="120px">
        <template v-slot="scope">
          <span>{{ scope.row.specification | emptyTextFormatter }}</span>
        </template>
      </el-table-column>
      <el-table-column key="material" prop="material" align="center" label="材质">
        <template v-slot="scope">
          <span>{{ scope.row.material | emptyTextFormatter }}</span>
        </template>
      </el-table-column>
      <el-table-column key="measurementUnit" prop="measurementUnit" align="center" label="计量单位" width="80">
        <template v-slot="scope">
          <span>{{ scope.row.measurementUnit | emptyTextFormatter }}</span>
        </template>
      </el-table-column>
      <el-table-column key="quantity" prop="quantity" label="数量" align="center" />
      <el-table-column key="checkUnit" prop="checkUnit" align="center" label="核算单位" width="80">
        <template v-slot="scope">
          <span>{{ scope.row.checkUnit | emptyTextFormatter }}</span>
        </template>
      </el-table-column>
      <el-table-column key="totalMete" prop="totalMete" align="center" :label="`总量`" min-width="120">
        <template v-slot="scope">
          <span>{{ scope.row.totalMete | emptyTextFormatter }}</span>
        </template>
      </el-table-column>
      <el-table-column key="price" prop="price" label="单价（元）" align="center">
        <template v-slot="scope">
          {{ scope.row.price | toFixed($DP.YUAN) | emptyTextFormatter }}
        </template>
      </el-table-column>
      <el-table-column key="totalPrice" prop="totalPrice" label="总额（元）" align="center">
        <template v-slot="scope">
          {{ scope.row.totalPrice | toFixed($DP.YUAN) | emptyTextFormatter }}
        </template>
      </el-table-column>
    </el-table>
  </div>
</template>

<script>
import { deliverDetail } from '@/api/contract/costCenter/financial-report'
import sizeCalc from '@/mixins/sizeCalc'

export default {
  mixins: [sizeCalc],
  props: {
    projectId: {
      type: [String, Number],
      default: undefined
    }
  },
  data() {
    return {
      extraHeight: 250,
      loading: false,
      list: []
    }
  },
  watch: {
    'projectId': {
      handler(val) {
        this.fetchDetail()
      },
      immediate: true
    }
  },
  methods: {
    async fetchDetail() {
      let _list = []
      this.loading = true
      try {
        const { content = [] } = await deliverDetail(this.projectId)
        _list = content
      } catch (error) {
        console.log('发货额', error)
      } finally {
        this.loading = false
        this.list = _list
      }
    },
    getSummaries(param) {
      const { columns, data } = param
      const sums = []
      columns.forEach((column, index) => {
        if (index === 0) {
          sums[index] = '合计'
          return
        }
        if (column.property === 'quantity' || column.property === 'totalPrice' || column.property === 'totalMete') {
          const values = data.map(item => Number(item[column.property]))
          if (!values.every(value => isNaN(value))) {
            sums[index] = values.reduce((prev, curr) => {
              const value = Number(curr)
              if (!isNaN(value)) {
                return prev + curr
              } else {
                return prev
              }
            }, 0)
            if (column.property === 'totalMete' || column.property === 'totalPrice') {
              sums[index] = sums[index].toFixed(column.property === 'totalPrice' ? this.$DP.YUAN : 5)
            }
          }
        }
      })
      return sums
    }
  }
}
</script>
