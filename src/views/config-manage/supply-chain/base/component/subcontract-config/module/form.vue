<template>
  <common-drawer
    append-to-body
    :close-on-click-modal="false"
    :before-close="crud.cancelCU"
    :visible="crud.status.cu > 0"
    :title="crud.status.title"
    :wrapper-closable="false"
    size="860px"
  >
    <template #titleRight>
      <common-button :loading="crud.status.cu === 2" type="primary" size="mini" @click="crud.submitCU">确认</common-button>
    </template>
    <template #content>
      <el-form ref="formRef" :model="form" size="small" label-width="140px">
        <common-table
          ref="detailRef"
          border
          :data="form.list"
          :max-height="300"
          style="width: 100%"
          class="table-form"
          return-source-data
          :showEmptySymbol="false"
          :cell-class-name="wrongCellMask"
        >
          <el-table-column label="序号" type="index" align="center" width="50" />
          <el-table-column prop="name" label="分包类别" align="center" min-width="270">
            <template v-slot="scope">
              <el-input v-model="scope.row.name" type="text" placeholder="分包类别" style="width: 260px" maxlength="50"/>
            </template>
          </el-table-column>
          <el-table-column label="操作" align="center">
            <template v-slot="scope">
              <common-button size="small" class="el-icon-delete" type="danger" @click="deleteRow(scope.$index)" />
            </template>
          </el-table-column>
        </common-table>
        <div class="add-row-box">
          <common-button
            size="mini"
            icon="el-icon-circle-plus-outline"
            type="warning"
            style="margin-right: 15px"
            @click="addRow()"
            >继续添加</common-button>
        </div>
      </el-form>
    </template>
  </common-drawer>
</template>

<script setup>
import { ref } from 'vue'
import { regForm } from '@compos/use-crud'

import { useStore } from 'vuex'
import { validate } from '@compos/form/use-table-validate'
import { ElMessage } from 'element-plus'

const store = useStore()
const formRef = ref()
const detailRef = ref()
const defaultForm = {
  list: []
}

const { CRUD, crud, form } = regForm(defaultForm, formRef)

const tableRules = {
  name: [{ required: true, message: '请输入分包类别', trigger: 'blur' }]
}

function wrongCellMask({ row, column }) {
  if (!row) return
  const rules = tableRules
  let flag = true
  if (row.verify && Object.keys(row.verify) && Object.keys(row.verify).length > 0) {
    if (row.verify[column.property] === false) {
      flag = validate(column.property, rules[column.property], row)
    }
    if (flag) {
      row.verify[column.property] = true
    }
  }
  return flag ? '' : 'mask-td'
}

function deleteRow(index) {
  form.list.splice(index, 1)
}

function addRow() {
  form.list.push({
    name: undefined
  })
}

CRUD.HOOK.beforeValidateCU = (crud, form) => {
  if (crud.form.list.length <= 0) {
    ElMessage({ message: '请先填写分包类别明细', type: 'error' })
    return false
  }
  const rules = tableRules
  let flag = true
  crud.form.list.map(row => {
    row.verify = {}
    for (const rule in rules) {
      row.verify[rule] = validate(rule, rules[rule], row)
      if (!row.verify[rule]) {
        flag = false
      }
    }
  })
  if (!flag) {
    ElMessage.error('请填写表格中标红数据')
    return false
  }
}

async function handleSuccess() {
  try {
    await store.dispatch('config/fetchSubcontractType')
  } catch (e) {
    console.log(e)
  }
}
CRUD.HOOK.afterSubmit = () => {
  handleSuccess()
}
</script>
<style lang="scss" scoped>
::v-deep(.el-input-number .el-input__inner) {
  text-align: left;
}
.table-form {
  ::v-deep(.el-input__inner) {
    padding: 0;
    padding-left: 5px;
  }
}
.add-row-box {
  text-align: center;
  margin-top: 20px;
}
</style>
