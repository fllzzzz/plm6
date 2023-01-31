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
          <el-row :gutter="20" class="panel-group">
            <el-col :span="8" class="card-panel-col">
              <Panel name="合同额/结算额" text-color="#626262" num-color="#1890ff" :end-val="detailRow.settlementAmount|| detailRow.contractAmount || 0" :precision="2" />
            </el-col>
            <el-col :span="8" class="card-panel-col">
              <Panel name="综合成本" text-color="#626262" num-color="#F56C6C" :end-val="detailRow.costAmount || 0" :precision="2" />
            </el-col>
            <el-col :span="8" class="card-panel-col">
              <Panel name="毛利润" text-color="#626262" num-color="#1890ff" :end-val="detailRow.grossProfit || 0" :precision="2" />
            </el-col>
          </el-row>
          <el-divider><span class="title">直接费用</span></el-divider>
          <common-table
            ref="directRef"
            highlight-current-row
            :data="directCostList"
            style="width: 100%"
            row-key="id"
            :data-format="dataFormat"
            @row-click="handleRowChange"
          >
            <el-table-column prop="index" label="序号" align="center" width="60" type="index" />
            <el-table-column prop="name" key="name" label="项目" align="center" />
            <el-table-column prop="amount" key="amount" label="金额（元）" align="center" />
            <el-table-column prop="rate" key="rate" label="占比" align="center">
              <template v-slot="scope">
                <span>{{ scope.row.rate }}%</span>
              </template>
            </el-table-column>
          </common-table>
          <el-divider><span class="title">间接费用</span></el-divider>
          <common-table
            ref="indirectRef"
            highlight-current-row
            :data="indirectCostList"
            style="width: 100%"
            row-key="id"
            :data-format="dataFormat"
            @row-click="handleRowChange"
          >
            <el-table-column prop="index" label="序号" align="center" width="60" type="index" />
            <el-table-column prop="name" key="name" label="项目" align="center" />
            <el-table-column prop="amount" key="amount" label="金额（元）" align="center" />
            <el-table-column prop="rate" key="rate" label="占比" align="center">
              <template v-slot="scope">
                <span>{{ scope.row.rate }}%</span>
              </template>
            </el-table-column>
          </common-table>
        </div>
        <div style="border-right: 1px solid #ededed; margin: 0 20px; height: calc(100vh - 180px)"></div>
        <div style="flex: 1">
          <div v-if="!costTypeData.key" class="my-code">*点击左侧表格行查看详情</div>
          <div v-if="costTypeData.key">
            <component :is="currentView" ref="detailRef" :cost-type-data="costTypeData" :permission="permission"/>
          </div>
        </div>
      </div>
    </template>
  </common-drawer>
</template>

<script setup>
import useVisible from '@compos/use-visible'
import { defineProps, defineEmits, ref, watch, computed } from 'vue'

import checkPermission from '@/utils/system/check-permission'
import { fortuneReportPM } from '@/page-permission/contract'

import mainMaterialFee from './module/main-material-fee.vue'
import laborFee from './module/labor-fee.vue'
import auxiliaryMaterialFee from './module/auxiliary-material-fee.vue'
// import waterElectricFee from './module/water-electric-fee.vue'
import depreciationFee from './module/depreciation-fee.vue'
// import managementFee from './module/management-fee.vue'
import shippingFee from './module/shipping-fee.vue'
import testingFee from './module/testing-fee.vue'
import subcontractingFee from './module/subcontracting-fee.vue'
import Panel from '@/components/Panel'

const permission = fortuneReportPM.cost
const costTypeData = ref({})
const detailRef = ref()
const directRef = ref()
const indirectRef = ref()
const directCostList = ref([
  { key: 'mainMaterialFee', name: '主材费', components: mainMaterialFee },
  { key: 'laborFee', name: '人工费', components: laborFee },
  { key: 'auxiliaryMaterialFee', name: '辅材费', components: auxiliaryMaterialFee },
  { key: 'transportationFee', name: '运输费', components: shippingFee },
  { key: 'testingFee', name: '检测费', components: testingFee },
  { key: 'subFee', name: '分包费', components: subcontractingFee }
  // { key: 'projectManagementFee', name: '项目管理费', components: mainMaterialFee }
])
const indirectCostList = ref([
  // { key: 'waterElectricityGasFee', name: '水电费', components: waterElectricFee },
  { key: 'depreciationFee', name: '折旧费', components: depreciationFee }
  // { key: 'managementFee', name: '管理费', components: managementFee }
])

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

const dataFormat = ref([
  ['rate', ['to-fixed', 2]],
  ['amount', 'to-thousand']
])

watch(
  () => props.detailRow.id,
  (value) => {
    directCostList.value.map(v => {
      v.amount = props.detailRow.compositeCostDTO && props.detailRow.compositeCostDTO[v.key] ? props.detailRow.compositeCostDTO[v.key] : 0
      v.rate = v.amount && props.detailRow.costAmount ? (v.amount / props.detailRow.costAmount) * 100 : 0
      v.projectId = props.detailRow.id
      return v
    })
    indirectCostList.value.map(v => {
      v.amount = props.detailRow.compositeCostDTO && props.detailRow.compositeCostDTO[v.key] ? props.detailRow.compositeCostDTO[v.key] : 0
      v.rate = v.amount && props.detailRow.costAmount ? (v.amount / props.detailRow.costAmount) * 100 : 0
      v.projectId = props.detailRow.id
      return v
    })
  },
  { immediate: true }
)

// 当前显示组件
const currentView = computed(() => {
  return costTypeData.value.components
})

const { visible: drawerVisible, handleClose } = useVisible({ emit, props, field: 'visible', showHook: showHook })

function showHook() {
  costTypeData.value = {}
}

function handleRowChange(row) {
  if (!checkPermission(permission.detail)) {
    return false
  }
  costTypeData.value = row
}
</script>

<style lang="scss" scoped>
.panel-group {
  margin-bottom:10px;
  ::v-deep(.card-panel) {
    .card-panel-description {
      .card-panel-text {
        text-align:center;
        margin-top: 2px;
      }
      .card-panel-num {
        display:block;
        font-size: 18px;
        text-align:center;
      }
    }
  }
}
</style>
