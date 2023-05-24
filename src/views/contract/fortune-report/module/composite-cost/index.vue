<template>
  <common-drawer
    customClass="composite-cost-drawer"
    :close-on-click-modal="false"
    append-to-body
    v-model="visible"
    :before-close="handleDrawerClose"
    :title="`成本明细 ${props.detailRow.project}`"
    :wrapper-closable="true"
    size="100%"
  >
    <template #content>
      <div class="wrap">
        <div class="wrap-left">
          <div class="amount-wrap">
            <el-card body-style="padding: 10px 20px" class="amount-info">
              <span>合同金额</span>
              <span>
                <span style="color: #0079ff">{{ props.detailRow.contractAmount }}</span> 元
              </span>
            </el-card>
            <el-card body-style="padding: 10px 20px" class="amount-info">
              <span>收款金额</span>
              <span>
                <span style="color: #0079ff">{{ props.detailRow.collectionAmount }}</span> 元
              </span>
            </el-card>
            <el-card body-style="padding: 10px 20px" class="amount-info">
              <span>综合成本</span>
              <span>
                <span style="color: #ff5600">{{ props.detailRow.costAmount }}</span> 元
              </span>
            </el-card>
            <el-card body-style="padding: 10px 20px" class="amount-info">
              <span>毛利润</span>
              <span>
                <span style="color: #1abc3c">{{ props.detailRow.grossProfit }}</span> 元
              </span>
            </el-card>
            <el-card body-style="padding: 10px 20px" class="amount-info">
              <span>净利润</span>
              <span>
                <span style="color: #1abc3c">{{ props.detailRow.retainedProfit }}</span> 元
              </span>
            </el-card>
          </div>
          <div class="cost-wrap" :style="{ maxHeight: maxHeight - 265 + 'px', overflowY: 'auto' }">
            <!-- 费用归属 -->
            <template v-for="item in costList" :key="item.type">
              <el-divider>
                <span class="title">
                  <span>{{ item.title }} | 合计：<span v-thousand="item.totalAmount" /></span>
                  <span v-if="item.showRate"> | {{ item.totalRate }}%</span>
                </span>
              </el-divider>
              <common-table
                :data="item.list"
                :data-format="columnsDataFormat"
                @row-click="handleRowChange"
                style="margin-bottom: 10px"
              >
                <el-table-column type="index" prop="index" label="序号" align="center" width="60px" />
                <el-table-column key="name" prop="name" label="项目" show-overflow-tooltip align="center" />
                <el-table-column key="amount" prop="amount" label="金额（元）" show-overflow-tooltip align="center" />
                <el-table-column v-if="item.showRate" key="rate" prop="rate" label="占比" show-overflow-tooltip align="center" />
              </common-table>
            </template>
          </div>
        </div>
        <div class="wrap-right">
          <component
            v-if="currentRow.expenseClassEnum"
            :is="currentView"
            :maxHeight="maxHeight"
            :detail-row="{ ...currentRow, projectId: props.detailRow.id, costAmount: props.detailRow.sourceRow.costAmount }"
          />
          <div class="hint" v-else-if="costList.length">点击左侧表格行查看详情</div>
          <div class="hint" v-else>暂无数据</div>
        </div>
      </div>
    </template>
  </common-drawer>
</template>

<script setup>
import { ref, watch, computed, defineEmits, defineProps } from 'vue'

import { costAscriptionEnum } from '@enum-ms/config'
import { expenseClassEnum } from '@enum-ms/contract'
import { toFixed } from '@data-type'

import useMaxHeight from '@compos/use-max-height'
import useVisible from '@compos/use-visible'
import costRecord from './cost-record'
import amortizationRecord from './amortization-record'

const props = defineProps({
  modelValue: {
    type: Boolean,
    require: true
  },
  detailRow: {
    type: Object,
    default: () => {}
  }
})

const costList = ref([])
const currentRow = ref({})
// 列格式转换
const columnsDataFormat = ref([
  ['amount', 'to-thousand'],
  ['rate', ['suffix', '%']]
])
// 摊销类型
const amortizationType = [
  expenseClassEnum.WATER.V,
  expenseClassEnum.INDUSTRIAL_ELECTRICITY.V,
  expenseClassEnum.CIVIL_ELECTRICITY.V,
  expenseClassEnum.WORKSHOP_DEPRECIATION.V,
  expenseClassEnum.EQUIPMENT_DEPRECIATION.V,
  expenseClassEnum.GAS.V
]

// 当前显示组件
const currentView = computed(() => {
  if (currentRow.value.expenseClassEnum) {
    // 费用类型
    if (currentRow.value.expenseClassEnum === expenseClassEnum.REIMBURSE_EXPENSES.V) {
      return costRecord
    }
    // 摊销类型
    if (amortizationType.includes(currentRow.value.expenseClassEnum)) {
      return amortizationRecord
    }
    // 材料记录
    return costRecord
  }
  return undefined
})

watch(
  () => props.detailRow,
  (row) => {
    costList.value = []
    const costAmount = props.detailRow.sourceRow.costAmount || 0
    for (const key in row.costMap) {
      if (row.costMap[key].length) {
        const data = {
          type: Number(key),
          title: costAscriptionEnum.VL[key],
          showRate: Number(key) === costAscriptionEnum.DIRECT_COSTS.V || Number(key) === costAscriptionEnum.INDIRECT_COSTS.V,
          totalAmount: 0
        }
        data.totalAmount = row.costMap[key].reduce((pre, cur) => {
          if (cur) {
            return pre + Number(cur.amount)
          } else {
            return pre
          }
        }, 0)
        data.totalRate = toFixed((data.totalAmount / costAmount) * 100, 2)
        data.list = row.costMap[key].map((v) => {
          v.showRate = data.showRate
          v.costAscriptionEnum = key
          v.costAscriptionName = data.title // 费用名称
          v.costAscriptionAmount = data.totalAmount // 费用总金额
          v.costAmount = costAmount // 综合成本
          if (data.showRate) {
            v.rate = toFixed((v.amount / costAmount) * 100, 2)
          }
          return v
        })
        costList.value.push(data)
      }
    }
  },
  { deep: true }
)

const emit = defineEmits(['update:modelValue'])
const { visible, handleClose } = useVisible({ emit, props })

const { maxHeight } = useMaxHeight(
  {
    mainBox: '.composite-cost-drawer',
    extraBox: ['.el-drawer__header'],
    wrapperBox: ['.el-drawer__body'],
    minHeight: 100
  },
  visible
)

function handleRowChange(row = {}) {
  currentRow.value = row
}

function handleDrawerClose() {
  handleRowChange()
  handleClose()
}
</script>

<style lang="scss" scoped>
.amount-info {
  color: #706f6f;
  font-weight: bold;
  margin-bottom: 10px;
  .el-card__body > span:last-child {
    float: right;
  }
}
::v-deep(.el-divider__text) {
  padding: 0;
}
.wrap {
  display: flex;
  height: 100%;
  .wrap-left {
    width: 600px;
    height: 100%;
    padding-right: 20px;
    margin-right: 20px;
    border-right: 1px solid #c8d2e6;
    .el-form {
      border-top: 1px solid #c8d2e6;
      .el-form-item {
        border: 1px solid #c8d2e6;
        border-top-width: 0;
        margin-bottom: 0;
        ::v-deep(label) {
          text-align: center;
          padding-right: 0;
          border-right: 1px solid #c8d2e6;
          background: #f3f8fd;
        }
        > :last-child {
          padding: 0 10px;
        }
      }
    }
  }
  .wrap-right {
    flex: 1;
    min-width: 300px;
    overflow-x: auto;
    .hint {
      background: #0079ff1a;
      border-left: 6px solid #0079ff;
      line-height: 40px;
      padding-left: 20px;
    }
  }
}
</style>
