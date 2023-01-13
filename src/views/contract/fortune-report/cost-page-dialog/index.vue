<template>
  <common-drawer
    ref="drawerRef"
    :title="`${detailRow.name} 成本页面详情`"
    v-model="drawerVisible"
    direction="rtl"
    :before-close="handleClose"
    size="100%"
  >
    <template #content>
      <div style="display: flex">
        <div style="width: 30%">
          <!--表格渲染-->
          <common-table
            ref="tableRef"
            :max-height="maxHeight"
            highlight-current-row
            :data="productionLineData"
            return-source-data
            style="width: 100%"
            row-key="id"
            @row-click="handleRowChange"
          >
            <el-table-column prop="index" label="序号" align="center" width="60" type="index" />
            <el-table-column prop="name" key="name" label="项目" align="center">
              <template v-slot="scope">
                <span>{{ scope.row.name }}</span>
              </template>
            </el-table-column>
            <el-table-column prop="amount" key="amount" label="金额（元）" align="center">
              <template v-slot="scope">
                <span>{{ toThousand(scope.row.amount) }}</span>
              </template>
            </el-table-column>
            <el-table-column prop="rate" key="rate" label="占比" align="center">
              <template v-slot="scope">
                <span>{{ scope.row.rate }}%</span>
              </template>
            </el-table-column>
          </common-table>
        </div>
        <div style="border-right: 1px solid #ededed; margin: 0 20px; height: calc(100vh - 180px)"></div>
        <div style="flex: 1">
          <div v-if="!costTypeData.id" class="my-code" style="width: 100%">*点击左侧表格行查看详情</div>
          <div v-if="costTypeData.id" style="width: 100%">
            <main-material-fee v-if="costTypeData.name === '主材费'" :cost-type-data="costTypeData" />
            <labor-fee v-else-if="costTypeData.name === '人工费'" :cost-type-data="costTypeData" />
            <auxiliary-material-fee v-else-if="costTypeData.name === '辅材费'" :cost-type-data="costTypeData" />
            <water-electric-fee v-else-if="costTypeData.name === '水电费'" :cost-type-data="costTypeData" />
            <depreciation-fee v-else-if="costTypeData.name === '折旧费'" :cost-type-data="costTypeData" />
            <management-fee v-else-if="costTypeData.name === '管理费'" :cost-type-data="costTypeData" />
            <shipping-fee v-else-if="costTypeData.name === '运输费'" :cost-type-data="costTypeData" />
            <testing-fee v-else-if="costTypeData.name === '检测费'" :cost-type-data="costTypeData" />
            <subcontracting-fee v-else-if="costTypeData.name === '分包费'" :cost-type-data="costTypeData" />
          </div>
        </div>
      </div>
    </template>
  </common-drawer>
</template>

<script setup>
import useVisible from '@compos/use-visible'
import useMaxHeight from '@compos/use-max-height'
import { toThousand } from '@data-type/number'
import { defineProps, defineEmits, ref } from 'vue'
import mainMaterialFee from './module/main-material-fee.vue'
import laborFee from './module/labor-fee.vue'
import auxiliaryMaterialFee from './module/auxiliary-material-fee.vue'
import waterElectricFee from './module/water-electric-fee.vue'
import depreciationFee from './module/depreciation-fee.vue'
import managementFee from './module/management-fee.vue'
import shippingFee from './module/shipping-fee.vue'
import testingFee from './module/testing-fee.vue'
import subcontractingFee from './module/subcontracting-fee.vue'

const costTypeData = ref({})
const emit = defineEmits(['update:visible'])
const props = defineProps({
  visible: {
    type: Boolean,
    default: false
  },
  detailRow: {
    type: Object,
    default: () => {}
  }
})

const productionLineData = [
  { id: 1, name: '主材费', amount: '100000', rate: 18 },
  { id: 2, name: '人工费', amount: '110000', rate: 18 },
  { id: 3, name: '辅材费', amount: '120000', rate: 18 },
  { id: 4, name: '水电费', amount: '130000', rate: 18 },
  { id: 5, name: '折旧费', amount: '140000', rate: 18 },
  { id: 6, name: '管理费', amount: '150000', rate: 18 },
  { id: 7, name: '运输费', amount: '160000', rate: 18 },
  { id: 8, name: '检测费', amount: '170000', rate: 18 },
  { id: 9, name: '分包费', amount: '180000', rate: 18 },
  { id: 10, name: '项目管理费', amount: '190000', rate: 18 }
]
const { maxHeight } = useMaxHeight({
  extraBox: ['.head-container'],
  paginate: true
})

const { visible: drawerVisible, handleClose } = useVisible({ emit, props, field: 'visible', showHook: showHook })

function showHook() {
  costTypeData.value = {}
}

function handleRowChange(row) {
  console.log(row, 'row')
  costTypeData.value = row
}
</script>

<style rel="stylesheet/scss" lang="scss" scoped>
</style>
