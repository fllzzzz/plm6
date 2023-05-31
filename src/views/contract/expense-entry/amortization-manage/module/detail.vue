<template>
  <common-drawer
    customClass="amortization-detail-drawer"
    :close-on-click-modal="false"
    append-to-body
    v-model="visible"
    :before-close="handleClose"
    title="摊销详情"
    :wrapper-closable="true"
    size="70%"
  >
    <template #content>
      <el-card class="amortization-detail">
        <div>{{ props.detailRow.name }}</div>
        <div>
          时间 <span class="blue">{{ props.detailRow.date }}</span>
        </div>
        <div>
          摊销金额 <span class="blue">{{ props.detailRow.amount }} 元</span>
        </div>
        <div>
          摊销产量 <span class="blue">{{ props.detailRow.productMete }} 吨</span>
        </div>
      </el-card>
      <common-table ref="tableRef" v-loading="tableLoading" :data-format="columnsDataFormat" :data="list" :max-height="maxHeight">
        <el-table-column label="序号" type="index" align="center" width="60" />
        <el-table-column align="center" key="project" prop="project" :show-overflow-tooltip="true" label="项目" />
        <el-table-column align="center" key="productMete" prop="productMete" :show-overflow-tooltip="true" label="产量（吨）" />
        <el-table-column align="center" key="amount" prop="amount" :show-overflow-tooltip="true" label="摊销金额" />
        <el-table-column align="center" key="proportion" prop="proportion" :show-overflow-tooltip="true" label="摊销占比" />
      </common-table>
    </template>
  </common-drawer>
</template>

<script setup>
import { detail } from '@/api/contract/expense-entry/amortization-manage'
import { defineProps, defineEmits, ref, watch } from 'vue'

import { toFixed } from '@/utils/data-type'
import { convertUnits } from '@/utils/convert/unit'
import { DP } from '@/settings/config'

import useMaxHeight from '@compos/use-max-height'
import useVisible from '@compos/use-visible'

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

const emit = defineEmits(['update:modelValue'])
const { visible, handleClose } = useVisible({ emit, props })

const { maxHeight } = useMaxHeight(
  {
    mainBox: '.amortization-detail-drawer',
    extraBox: ['.el-drawer__header', '.amortization-detail'],
    wrapperBox: ['.el-drawer__body']
  },
  visible
)

watch(
  () => visible.value,
  (val) => {
    if (val) {
      fetchList()
    }
  }
)

const tableLoading = ref(false)
const list = ref([])
const columnsDataFormat = ref([
  ['project', ['parse-project']],
  ['amount', 'to-thousand'],
  ['proportion', ['suffix', ' %']]
])

async function fetchList() {
  let _list = []
  try {
    tableLoading.value = true
    _list = (await detail({ id: props.detailRow.id })) || []
    _list.forEach((row) => {
      row.project = { id: row.projectId, name: row.projectName, serialNumber: row.serialNumber, shortName: row.shortName }
      row.proportion = toFixed((row.proportion *= 100), 2)
      row.productMete = convertUnits(row.productMete, 'kg', 't', DP.CONTRACT_WT__T)
    })
  } catch (error) {
    console.log('获取摊销详情失败')
  } finally {
    list.value = _list
    tableLoading.value = false
  }
}
</script>

<style lang="scss" scoped>
.amortization-detail {
  margin-bottom: 20px;
  ::v-deep(.el-card__body) {
    display: flex;
    > div:first-child {
      color: #706f6f;
      font-weight: bold;
      padding-right: 30px;
    }
    > div:not(:first-child) {
      flex: 1;
      text-align: center;
      border-left: 1px solid #ebeef5;
      .blue {
        color: #0079ff;
        font-weight: bold;
      }
    }
  }
}
</style>
