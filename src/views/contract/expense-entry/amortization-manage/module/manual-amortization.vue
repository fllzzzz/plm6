<template>
  <common-drawer
    customClass="manual-amortization-drawer"
    :close-on-click-modal="false"
    append-to-body
    v-model="visible"
    :before-close="handleClose"
    title="待摊销列表"
    :wrapper-closable="true"
    size="70%"
  >
    <template #titleRight>
      <el-popconfirm
        confirm-button-text="确定"
        cancel-button-text="取消"
        icon="el-icon-info"
        title="摊销后将不能撤回，确认摊销吗？"
        @confirm="submit(true)"
      >
        <template #reference>
          <common-button type="primary" size="mini">一键摊销</common-button>
        </template>
      </el-popconfirm>
    </template>
    <template #content>
      <common-table :data="list" row-key="id" tree-default-expand-all :data-format="columnsDataFormat" :indent="0" :max-height="maxHeight">
        <el-table-column prop="index" key="index" label="序号" align="center" width="80" />
        <el-table-column prop="date" key="date" label="摊销时间段" align="center">
          <template #default="{ row }">
            <span :class="{ parentElement: row.isParent }">{{ row.date }}</span>
          </template>
        </el-table-column>
        <el-table-column key="amortizationClassName" prop="amortizationClassName" label="摊销类型" align="center">
          <template #default="{ row }">
            <span>
              <span v-if="row.fullPathName" style="color: #adadad">{{ row.fullPathName }} > </span>{{ row.amortizationClassName }}
            </span>
          </template>
        </el-table-column>
        <el-table-column key="totalAmount" prop="totalAmount" label="摊销金额" align="center" />
        <el-table-column key="projectMete" prop="projectMete" :show-overflow-tooltip="true" label="预计摊销产量（吨）" align="center" />
        <el-table-column align="center" label="操作" width="80px">
          <template #default="{ row }">
            <el-popconfirm
              v-if="row.isParent"
              confirm-button-text="确定"
              cancel-button-text="取消"
              icon="el-icon-info"
              title="摊销后将不能测回，确认摊销吗？"
              @confirm="submit(false, row)"
            >
              <template #reference>
                <common-button type="primary" size="mini">摊销</common-button>
              </template>
            </el-popconfirm>
          </template>
        </el-table-column>
      </common-table>
    </template>
  </common-drawer>
</template>

<script setup>
import { getManualAmortization, singleAmortize, amortizationAll } from '@/api/contract/expense-entry/amortization-manage'
import { ref, defineEmits, defineProps, watch } from 'vue'

import moment from 'moment'
import { debounce } from '@/utils'
import { expenseClassEnum } from '@enum-ms/contract'

import useMaxHeight from '@compos/use-max-height'
import useVisible from '@compos/use-visible'
import { ElMessage } from 'element-plus'

const props = defineProps({
  modelValue: {
    type: Boolean,
    require: true
  }
})

const list = ref([])
const listLoading = ref(false)

const emit = defineEmits(['success', 'update:modelValue'])
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
    mainBox: '.manual-amortization-drawer',
    extraBox: ['.el-drawer__header'],
    wrapperBox: ['.el-drawer__body']
  },
  visible
)

// 摊销
const submit = debounce(
  async function (flag, row) {
    try {
      if (flag) {
        await amortizationAll()
      } else {
        await singleAmortize({ expenseClassEnum: row.expenseClassEnum, ids: row.ids })
      }
      ElMessage.success('摊销成功')
      getList()
      emit('success')
    } catch (error) {
      console.log('摊销失败', error)
    }
  },
  200,
  false
)

// 获取列表
async function getList() {
  try {
    listLoading.value = true
    const data = (await getManualAmortization()) || []
    list.value = data.map((row, index) => {
      const _startDate = moment(row.startDate).format('YYYY-MM-DD')
      const _endDate = moment(row.endDate).format('YYYY-MM-DD')
      row.date = `${_startDate} ~ ${_endDate}`
      row.index = index + 1
      row.isParent = true
      const _name = expenseClassEnum.VL[row.expenseClassEnum]
      if (_name !== row.amortizationClassName) {
        row.fullPathName = _name
      }
      // 汇总数据没有id
      if (!row.id) {
        row.id = Math.random()
      }
      row.children?.forEach((v, i) => {
        const _startDate = moment(v.startDate).format('YYYY-MM-DD')
        const _endDate = moment(v.endDate).format('YYYY-MM-DD')
        v.date = `${_startDate} ~ ${_endDate}`
        v.index = `${row.index}-${i + 1}`
        if (row.fullPathName) {
          v.amortizationClassName = row.amortizationClassName
          v.fullPathName = row.fullPathName
        }
      })
      return row
    })
  } catch (error) {
    console.log('获取手动摊销列表失败', error)
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
