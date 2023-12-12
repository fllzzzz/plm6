<template>
  <common-drawer
    customClass="available-balance-drawer"
    :close-on-click-modal="false"
    append-to-body
    v-model="visible"
    :before-close="handleDrawerClose"
    :title="`可用余额 ${props.detailRow.project}`"
    :wrapper-closable="true"
    size="100%"
  >
    <template #content>
      <div class="wrap">
        <div class="wrap-left">
          <el-divider>
            <span class="title">合同信息</span>
          </el-divider>
          <el-form size="small" label-width="100px">
            <el-form-item label="合同编号">
              <div>{{ props.detailRow.serialNumber }}</div>
            </el-form-item>
            <el-form-item label="项目名称">
              <div>{{ props.detailRow.name }}</div>
            </el-form-item>
            <el-form-item label="项目类型">
              <div>{{ props.detailRow.projectType }}</div>
            </el-form-item>
            <el-form-item label="业务类型">
              <div>{{ props.detailRow.businessType }}</div>
            </el-form-item>
            <el-form-item label="业主名称">
              <div>{{ props.detailRow.customerUnit }}</div>
            </el-form-item>
            <el-form-item label="签订日期">
              <div>{{ props.detailRow.signingDate }}</div>
            </el-form-item>
            <el-form-item label="项目经理">
              <div>{{ props.detailRow.projectManager }}</div>
            </el-form-item>
            <el-form-item label="合同金额">
              <div>
                <span style="color: #0079ff">{{ props.detailRow.contractAmount }}</span> 元
              </div>
            </el-form-item>
          </el-form>
          <el-divider>
            <span class="title">可用余额信息</span>
          </el-divider>
          <el-form size="small" label-width="100px">
            <el-form-item label="收款金额">
              <div class="pointer" @click="openDetail({ type: 'collection' })">{{ props.detailRow.collectionAmount }} 元</div>
            </el-form-item>
            <el-form-item label="出口退税">
              <div class="pointer" @click="openDetail({ type: 'exportTax' })">{{ props.detailRow.exportTaxRebate }} 元</div>
            </el-form-item>
            <el-form-item label="期间费用">
              <div
                class="pointer"
                @click="
                  openDetail({
                    type: 'costAscription',
                    costAscriptionEnum: costAscriptionEnum.PERIOD_COSTS.V,
                    costAscriptionAmount: props.detailRow.sourceRow.periodExpense,
                  })
                "
              >
                {{ props.detailRow.periodExpense }} 元
              </div>
            </el-form-item>
            <el-form-item label="累计发货金额">
              <div class="pointer" @click="openDetail({ type: 'happened' })">{{ props.detailRow.happenedAmount }} 元</div>
            </el-form-item>
            <el-form-item label="项目留存">
              <div
                class="pointer"
                @click="
                  openDetail({
                    type: 'costAscription',
                    costAscriptionEnum: costAscriptionEnum.PROJECT_RETENTION.V,
                    costAscriptionAmount: props.detailRow.sourceRow.projectRetention,
                  })
                "
              >
                {{ props.detailRow.projectRetention }} 元
              </div>
            </el-form-item>
            <el-form-item label="可用余额">
              <div>
                <span style="color: #0079ff">{{ props.detailRow.availableBalance }}</span> 元
              </div>
            </el-form-item>
            <el-form-item label="安全发货余额">
              <div>
                <span style="color: #05b975">{{ props.detailRow.safetyBalance }}</span> 元
              </div>
            </el-form-item>
          </el-form>
        </div>
        <div class="wrap-right">
          <component
            v-if="currentRow.type"
            :is="currentView"
            :maxHeight="maxHeight"
            :detail-row="{ ...props.detailRow, costAscription: { ...currentRow } }"
            :secondPickerTime="props.secondPickerTime"
          />
          <div class="hint" v-else>可通过点击【收款】、【出口退税】、【期间费用】、【累计发货金额】表格行进行对应项的明细查看</div>
        </div>
      </div>
    </template>
  </common-drawer>
</template>

<script setup>
import { ref, computed, defineEmits, defineProps } from 'vue'

import { costAscriptionEnum } from '@enum-ms/config'

import useMaxHeight from '@compos/use-max-height'
import useVisible from '@compos/use-visible'
import collection from './collection-record'
import happened from './happened-record'
import exportTax from './export-tax-rebate'
import costAscription from './cost-ascription'

const props = defineProps({
  modelValue: {
    type: Boolean,
    require: true
  },
  detailRow: {
    type: Object,
    default: () => {}
  },
  secondPickerTime: {
    type: Object,
    default: () => {}
  }
})

// 当前显示组件
const currentView = computed(() => {
  if (currentRow.value.type === 'collection') {
    return collection
  } else if (currentRow.value.type === 'happened') {
    return happened
  } else if (currentRow.value.type === 'exportTax') {
    return exportTax
  } else if (currentRow.value.type === 'costAscription') {
    return costAscription
  }
  return undefined
})

const currentRow = ref({})

const emit = defineEmits(['update:modelValue'])
const { visible, handleClose } = useVisible({ emit, props })

const { maxHeight } = useMaxHeight(
  {
    mainBox: '.available-balance-drawer',
    extraBox: ['.el-drawer__header'],
    wrapperBox: ['.el-drawer__body']
  },
  visible
)

function openDetail(row) {
  currentRow.value = row
}

function handleDrawerClose() {
  currentRow.value = {}
  handleClose()
}
</script>

<style lang="scss" scoped>
.wrap {
  display: flex;
  height: 100%;
  .wrap-left {
    width: 500px;
    height: 100%;
    overflow-y: auto;
    padding-right: 20px;
    padding-bottom: 20px;
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
    .hint {
      background: #0079ff1a;
      border-left: 6px solid #0079ff;
      line-height: 40px;
      padding-left: 20px;
    }
  }
}
</style>
