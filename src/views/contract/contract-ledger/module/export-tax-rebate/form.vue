<template>
  <common-drawer
    append-to-body
    :close-on-click-modal="false"
    :before-close="crud.cancelCU"
    :visible="crud.status.cu > 0"
    :title="crud.status.title"
    :wrapper-closable="false"
    size="100%"
  >
  <template #titleAfter>
      <el-tag size="medium" effect="plain">
        发货总额：
        <span v-thousand="props.currentRow.deliverInstallAmount"></span>
      </el-tag>
      <el-tag type="success" size="medium" effect="plain">
        已退税发货总额：
        <span v-thousand="props.currentRow.totalSendAmount"></span>
      </el-tag>
    </template>
    <template #titleRight>
      <common-button :loading="crud.status.cu === 2" type="primary" size="mini" @click="crud.submitCU">确认</common-button>
    </template>
    <template #content>
      <el-form ref="formRef" :model="form" size="small" label-width="140px">
        <common-table
          ref="detailRef"
          border
          size="mini"
          :data="form.list"
          :max-height="maxHeight"
          class="table-form"
          :cell-class-name="wrongCellMask"
          return-source-data
          :showEmptySymbol="false"
        >
          <el-table-column label="序号" type="index" align="center" width="50" />
          <el-table-column key="drawbackDate" prop="drawbackDate" label="退税日期" align="center" width="160">
            <template v-slot="{ row }">
              <el-date-picker
                v-model="row.drawbackDate"
                type="date"
                size="small"
                value-format="x"
                placeholder="请选择退税日期"
                style="width: 100%"
              />
            </template>
          </el-table-column>
          <el-table-column key="sendAmount" prop="sendAmount" label="发货额" align="center" min-width="85">
            <template v-slot="{ row }">
              <el-input-number
                v-show-thousand
                v-model.number="row.sendAmount"
                :min="0"
                :max="999999999999"
                :step="100"
                :precision="DP.YUAN"
                placeholder="请输入发货额"
                controls-position="right"
              />
            </template>
          </el-table-column>
          <el-table-column key="drawbackAmount" prop="drawbackAmount" label="退税总额" align="center" min-width="85">
            <template v-slot="{ row }">
              <el-input-number
                v-show-thousand
                v-model.number="row.drawbackAmount"
                :min="0"
                :max="999999999999"
                :step="100"
                :precision="DP.YUAN"
                placeholder="请输入退税总额"
                controls-position="right"
              />
            </template>
          </el-table-column>
          <el-table-column key="accountantId" prop="accountantId" label="核算人" align="center" min-width="85">
            <template v-slot="{ row }">
              <user-select
                v-model="row.accountantId"
                placeholder="请选择核算人"
                size="small"
                clearable
                style="width: 100%"
              />
            </template>
          </el-table-column>
          <el-table-column prop="remark" label="备注" align="center" min-width="130">
            <template v-slot="{ row }">
              <el-input v-model.trim="row.remark" type="text" placeholder="备注" style="width: 100%" />
            </template>
          </el-table-column>
          <el-table-column label="操作" align="center" width="90">
            <template v-slot="{ $index }">
              <common-button size="small" class="el-icon-delete" type="danger" @click="deleteRow($index)" />
            </template>
          </el-table-column>
        </common-table>
        <div class="add-row-box">
          <common-button size="mini" icon="el-icon-circle-plus-outline" type="warning" @click="addRow()"> 继续添加 </common-button>
        </div>
      </el-form>
    </template>
  </common-drawer>
</template>

<script setup>
import { ref, defineProps } from 'vue'

import { ElMessage } from 'element-plus'
import { DP } from '@/settings/config'

import useMaxHeight from '@compos/use-max-height'
import { regForm } from '@compos/use-crud'
import useTableValidate from '@compos/form/use-table-validate'
import userSelect from '@comp-common/user-select'

const formRef = ref()
const detailRef = ref()
const defaultForm = {
  list: []
}

const { CRUD, crud, form } = regForm(defaultForm, formRef)

const props = defineProps({
  currentRow: {
    type: Object,
    default: () => {}
  }
})

const { maxHeight } = useMaxHeight({
  extraBox: ['.el-drawer__header', '.add-row-box'],
  wrapperBox: ['.el-drawer__body']
})

const tableRules = {
  drawbackDate: [{ required: true, message: '请选择退税日期', trigger: 'change' }],
  accountantId: [{ required: true, message: '请选择核算人', trigger: 'change' }],
  sendAmount: [{ required: true, message: '请选择发货额', trigger: 'change', type: 'number' }],
  drawbackAmount: [{ required: true, message: '请选择退税总额', trigger: 'change', type: 'number' }]
}

const { tableValidate, wrongCellMask } = useTableValidate({ rules: tableRules }) // 表格校验

function deleteRow(index) {
  form.list.splice(index, 1)
}

function addRow() {
  form.list.push({
    projectId: props.currentRow.id,
    sendAmount: undefined,
    drawbackDate: undefined,
    accountantId: undefined
  })
}

CRUD.HOOK.beforeValidateCU = (crud, form) => {
  if (crud.form.list.length <= 0) {
    ElMessage({ message: '请添加出口退税明细', type: 'error' })
    return false
  }
  const { validResult, dealList } = tableValidate(crud.form.list)
  if (validResult) {
    crud.form.list = dealList
  } else {
    return validResult
  }
}
</script>
<style lang="scss" scoped>
.add-row-box {
  text-align: center;
  margin-top: 20px;
}
.table-form {
  ::v-deep(.el-table__cell .cell) {
    padding: 0 2px;
  }
}
</style>
