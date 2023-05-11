<template>
  <common-drawer
    customClass="auto-amortization-drawer"
    :close-on-click-modal="false"
    append-to-body
    v-model="visible"
    :before-close="handleClose"
    title="自动摊销列表"
    :wrapper-closable="true"
    size="70%"
  >
    <template #titleRight>
      <el-tag v-if="list.length" effect="plain" type="warning" size="medium">于 {{ autoAmortizationDate }} 自动摊销</el-tag>
    </template>
    <template #content>
      <common-table :data="list" row-key="id" tree-default-expand-all :data-format="columnsDataFormat" :max-height="maxHeight">
        <el-table-column prop="index" key="index" label="序号" align="center" width="80" />
        <el-table-column prop="date" key="date" label="摊销时间段" align="center">
          <template #default="{ row }">
            <span :class="{ parentElement: row.isParent }">{{ row.date }}</span>
          </template>
        </el-table-column>
        <el-table-column key="amortizationClassName" prop="amortizationClassName" label="摊销种类" align="center" />
        <el-table-column key="totalAmount" prop="totalAmount" label="摊销金额" align="center" />
        <el-table-column
          key="projectMete"
          prop="projectMete"
          :show-overflow-tooltip="true"
          label="预计摊销产量（吨）"
          align="center"
        />
      </common-table>
    </template>
  </common-drawer>
</template>

<script setup>
import { getAutoAmortization } from '@/api/contract/expense-entry/amortization-manage'
import { ref, defineEmits, defineProps, watch } from 'vue'

import moment from 'moment'

import useMaxHeight from '@compos/use-max-height'
import useVisible from '@compos/use-visible'

const props = defineProps({
  modelValue: {
    type: Boolean,
    require: true
  }
})

const list = ref([])
const listLoading = ref(false)
const autoAmortizationDate = ref()

const emit = defineEmits(['update:modelValue'])
const { visible, handleClose } = useVisible({ emit, props })

watch(
  () => visible.value,
  (val) => {
    if (val) {
      getList()
    }
  }
)

// 列格式转换
const columnsDataFormat = [['totalAmount', 'to-thousand']]

const { maxHeight } = useMaxHeight(
  {
    mainBox: '.auto-amortization-drawer',
    extraBox: ['.el-drawer__header'],
    wrapperBox: ['.el-drawer__body']
  },
  visible
)

// 获取列表
async function getList() {
  try {
    listLoading.value = true
    autoAmortizationDate.value = moment().endOf('months').format('MM月DD日HH:mm:ss')
    const data = (await getAutoAmortization()) || []
    list.value = data.map((row, index) => {
      const _startDate = moment(row.startDate).format('YYYY-MM-DD')
      const _endDate = moment(row.endDate).format('YYYY-MM-DD')
      row.date = `${_startDate} ~ ${_endDate}`
      row.index = index + 1
      row.isParent = true
      row?.children?.forEach((v, i) => {
        v.index = `${row.index}-${i + 1}`
      })
      return row
    })
  } catch (error) {
    console.log('获取自动摊销列表失败', error)
  } finally {
    listLoading.value = false
  }
}
</script>

<style lang="scss" scoped>
.parentElement {
  font-weight: bold;
}
</style>
