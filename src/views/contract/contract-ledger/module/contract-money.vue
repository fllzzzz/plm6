<template>
  <common-drawer
    append-to-body
    v-model="visible"
    top="10vh"
    width="600px"
    :before-close="handleClose"
    title="合同额"
    :wrapper-closable="false"
    size="900px"
  >
    <template #content>
      <common-table
        ref="tableRef"
        :data="tableData"
        :max-height="maxHeight"
        style="width: 100%"
        return-source-data
        :showEmptySymbol="false"
      >
      <el-table-column key="status" prop="status" :show-overflow-tooltip="true" label="项目" width="140px">
        <template v-slot="scope">
          <span>{{ scope.row.status }}</span>
        </template>
      </el-table-column>
      <el-table-column key="amount" prop="amount" :show-overflow-tooltip="true" label="金额" min-width="160px">
        <template v-slot="scope">
          <span>{{ scope.row.amount? toThousand(scope.row.amount): '-' }}</span>
        </template>
      </el-table-column>
      <el-table-column key="date" prop="date" :show-overflow-tooltip="true" label="日期" min-width="150px">
        <template v-slot="scope">
          <span>{{ scope.row.date? parseTime(scope.row.date,'{y}-{m}-{d}'): '-' }}</span>
        </template>
      </el-table-column>
      <el-table-column key="userName" prop="userName" :show-overflow-tooltip="true" label="操作人" min-width="150px">
        <template v-slot="scope">
          <span>{{ scope.row.userName }}</span>
        </template>
      </el-table-column>
    </common-table>
    </template>
  </common-drawer>
</template>

<script setup>
import { defineProps, defineEmits, watch, ref } from 'vue'
import useVisible from '@compos/use-visible'
import useMaxHeight from '@compos/use-max-height'
import { moneyChangeLog } from '@/api/contract/contract-ledger'
import { parseTime } from '@/utils/date'
import { toThousand } from '@data-type/number'

const props = defineProps({
  modelValue: {
    type: Boolean,
    require: true
  },
  projectId: {
    type: [String, Number],
    default: undefined
  }
})

const tableData = ref([])
const emit = defineEmits(['success', 'update:modelValue'])
const { visible, handleClose } = useVisible({ emit, props })
const { maxHeight } = useMaxHeight({
  wrapperBox: '.contractMoneyLog',
  paginate: true,
  extraHeight: 40
})

watch(
  () => props.projectId,
  (val) => {
    if (val) {
      getMoneyChangeLog()
    } else {
      tableData.value = []
    }
  },
  { deep: true, immediate: true }
)

async function getMoneyChangeLog() {
  try {
    const { content } = await moneyChangeLog({ projectId: props.projectId })
    tableData.value = content || []
  } catch (e) {
    console.log('获取变更金额记录', e)
  }
}
</script>
<style lang="scss" scoped>
::v-deep(.el-input-number .el-input__inner) {
  text-align: left;
}
</style>
