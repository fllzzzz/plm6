<template>
  <div>
    <el-radio-group v-model="status" size="small" @change="switchStatus">
      <el-radio-button label="出库">出库</el-radio-button>
      <el-radio-button label="还库">还库</el-radio-button>
    </el-radio-group>
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
      <el-table-column label="物料种类" min-width="180">
        <template v-slot="scope">
          <table-cell-tag v-if="scope.row.type" name="摊销" />
          <span>{{ scope.row.firstClassName+'/'+scope.row.secondClassName +'/' +scope.row.thirdClassName }}</span>
        </template>
      </el-table-column>
      <el-table-column prop="specification" label="规格">
        <template v-slot="scope">
          {{ scope.row.specification | emptyTextFormatter }}
        </template>
      </el-table-column>
      <el-table-column prop="unit" label="计量单位" align="center">
        <template v-slot="scope">
          {{ scope.row.unit | emptyTextFormatter }}
        </template>
      </el-table-column>
      <el-table-column prop="quantity" :label="quantityLabel" align="center">
        <template v-slot="scope">
          {{ scope.row.quantity | emptyTextFormatter }}
        </template>
      </el-table-column>
      <el-table-column prop="checkUnit" label="核算单位" align="center">
        <template v-slot="scope">
          {{ scope.row.checkUnit | emptyTextFormatter }}
        </template>
      </el-table-column>
      <el-table-column key="value" prop="value" :label="valueLabel" align="center">
        <template v-slot="scope">
          {{ scope.row.value | emptyTextFormatter }}
        </template>
      </el-table-column>
      <el-table-column key="averagePrice" prop="averagePrice" label="单价（元）" align="center">
        <template v-slot="scope">
          {{ scope.row.averagePrice | toFixed($DP.YUAN) | emptyTextFormatter }}
        </template>
      </el-table-column>
      <el-table-column key="costAmount" prop="costAmount" :label="costAmountLabel" align="center">
        <template v-slot="scope">
          {{ scope.row.costAmount | toFixed($DP.YUAN)| emptyTextFormatter }}
        </template>
      </el-table-column>
    </el-table>
  </div>
</template>

<script>
import { mainMaterial, auxiliaryMaterial, mainMeterialReturn, auxiliaryMaterialReturn } from '@/api/contract/costCenter/financial-report'
import sizeCalc from '@/mixins/sizeCalc'
import { meteFmtByBasicClass } from '@/utils/other'
import tableCellTag from '@/views/components/common/table-cell-tag'

const permission = {
  print: ['financial:print']
}

export default {
  components: { tableCellTag },
  mixins: [sizeCalc],
  props: {
    projectId: {
      type: [String, Number],
      default: undefined
    },
    id: {
      type: [String, Number],
      default: undefined
    }
  },
  data() {
    return {
      status: '出库',
      quantityLabel: '出库数',
      valueLabel: '出库量',
      costAmountLabel: '总额（元）',
      extraHeight: 250,
      loading: false,
      permission,
      list: []
    }
  },
  watch: {
    projectId() {
      this.fetchDetail()
    },
    'id': {
      handler(val) {
        this.status = '出库'
        this.fetchDetail()
      },
      immediate: true
    },
    status() {
      if (this.status === '出库') {
        this.quantityLabel = '出库数'
        this.valueLabel = '出库量'
        this.costAmountLabel = '总额（元）'
      } else {
        this.quantityLabel = '还库数'
        this.valueLabel = '还库量'
        this.costAmountLabel = '还库总额（元）'
      }
    }
  },
  methods: {
    switchStatus() {
      this.$emit('materialStatusChange', this.status)
      this.fetchDetail()
    },
    async fetchDetail() {
      let _list = []
      this.loading = true
      try {
        const { content = [] } = this.id === 'mainMaterial'
          ? (this.status === '出库' ? await mainMaterial(this.projectId) : await mainMeterialReturn(this.projectId))
          : (this.status === '出库' ? await auxiliaryMaterial(this.projectId) : await auxiliaryMaterialReturn(this.projectId))
        _list = content.map(v => {
          meteFmtByBasicClass({
            data: v,
            basicClass: v.basicClass,
            field: ['value']
          })
          if (v.number && v.unitPrice && v.totalAmount) {
            v.quantity = v.number
            v.averagePrice = v.unitPrice
            v.costAmount = v.totalAmount
          }
          return v
        })
      } catch (error) {
        console.log('材料成本', error)
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
        if (column.property === 'quantity' || column.property === 'value' || column.property === 'costAmount' || column.property === 'number' || column.property === 'totalAmount') {
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
            if (column.property === 'costAmount' || column.property === 'totalAmount') {
              sums[index] = sums[index].toFixed(this.$DP.YUAN)
            }
            if (column.property === 'value') {
              sums[index] = sums[index].toFixed(2)
            }
          }
        }
      })
      return sums
    }
  }
}
</script>
