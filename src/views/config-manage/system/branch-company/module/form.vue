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
      <el-form ref="formRef" :model="form" :rules="rules" size="small" label-width="140px">
        <el-form-item label="公司名称" prop="name">
          <el-input v-model="form.name" type="text" placeholder="请填写名称" class="input-underline" style="width: 320px" maxlength="50"/>
        </el-form-item>
        <el-form-item label="社会统一信用代码" prop="socialCode">
          <el-input
            v-model="form.socialCode"
            type="text"
            placeholder="请填写社会统一信用代码"
            class="input-underline"
            style="width: 320px"
            maxlength="18"
          />
        </el-form-item>
        <el-form-item label="母公司" prop="isParent">
          <template #label>
            <el-tooltip effect="light" content="只可选择一个公司为母公司，若选择新的母公司，原母公司会被取消" placement="left">
              <div style="display: inline-block">
                <span>母公司</span>
                <i class="el-icon-info" style="color: #909399" />
              </div>
            </el-tooltip>
          </template>
          <el-checkbox v-model="form.isParent" :true-label="systemEnabledEnum.ENUM.TRUE.V" :false-label="systemEnabledEnum.ENUM.FALSE.V" />
        </el-form-item>
        <el-form-item label="状态" prop="enabled">
          <common-radio v-model="form.enabled" :options="systemEnabledEnum.ENUM" type="enum" />
        </el-form-item>
        <el-form-item label="排序" prop="sort">
          <el-input-number v-model.number="form.sort" :min="1" :max="999" :step="1" controls-position="right" style="width: 270px" />
        </el-form-item>
        <el-form-item label="备注" prop="remark">
          <el-input
            v-model="form.remark"
            type="textarea"
            :autosize="{ minRows: 6, maxRows: 8 }"
            placeholder="请填写备注"
            maxlength="200"
            show-word-limit
            style="max-width: 500px"
          />
        </el-form-item>
        <common-table
          ref="detailRef"
          border
          :data="form.bankAccounts"
          :max-height="300"
          style="width: 100%"
          class="table-form"
          return-source-data
          :showEmptySymbol="false"
          :cell-class-name="wrongCellMask"
        >
          <el-table-column label="序号" type="index" align="center" width="50" />
          <el-table-column prop="depositBank" label="开户行" align="center" min-width="270">
            <template v-slot="scope">
              <el-input v-model="scope.row.depositBank" type="text" placeholder="开户行" style="width: 260px" maxlength="50"/>
            </template>
          </el-table-column>
          <el-table-column prop="account" label="账号" align="center" min-width="270">
            <template v-slot="scope">
              <el-input v-model="scope.row.account" type="text" placeholder="账号" style="width: 260px" maxlength="30"/>
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
import { validate } from '@compos/form/use-table-validate'
import { systemEnabledEnum } from '@enum-ms/system'
import { ElMessage } from 'element-plus'
import { validatorEnOrNum } from '@/utils/validate/pattern'

const formRef = ref()
const detailRef = ref()
const defaultForm = {
  bankAccounts: [],
  enabled: systemEnabledEnum.TRUE.V,
  isParent: systemEnabledEnum.FALSE.V,
  name: undefined,
  remark: undefined,
  socialCode: undefined,
  sort: undefined
}

const { CRUD, crud, form } = regForm(defaultForm, formRef)
const rules = {
  name: [{ required: true, message: '请输入公司名称', trigger: 'blur' }],
  sort: [{ required: true, message: '请输入排序', trigger: 'blur' }],
  socialCode: [
    { max: 18, message: '长度不超过 18 个字符', trigger: 'blur' },
    { pattern: validatorEnOrNum.pattern, message: validatorEnOrNum.message }
  ]
}

const tableRules = {
  depositBank: [{ required: true, message: '请输入开户行', trigger: 'blur' }]
  // account: [{ required: true, message: '请输入账号', trigger: 'blur' }]
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
  form.bankAccounts.splice(index, 1)
}

function addRow() {
  form.bankAccounts.push({
    account: undefined,
    depositBank: undefined
  })
}

CRUD.HOOK.beforeValidateCU = (crud, form) => {
  if (crud.form.bankAccounts.length <= 0) {
    ElMessage({ message: '请先填写银行账号明细', type: 'error' })
    return false
  }
  const rules = tableRules
  let flag = true
  crud.form.bankAccounts.map(row => {
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
