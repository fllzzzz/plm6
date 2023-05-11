<template>
  <common-dialog
    append-to-body
    :close-on-click-modal="false"
    :before-close="crud.cancelCU"
    :visible="crud.status.cu > 0"
    :title="crud.status.title"
    :show-close="false"
    width="490px"
    top="10vh"
  >
    <template #titleRight>
      <span style="float: right">
        <common-button :loading="crud.status.cu === CRUD.STATUS.PROCESSING" size="mini" type="primary" @click="crud.submitCU">
          提 交
        </common-button>
        <common-button size="mini" @click="crud.cancelCU">关 闭</common-button>
      </span>
    </template>
    <div class="form">
      <el-form ref="formRef" :model="form" :rules="rules" size="small" label-width="130px" class="demo-form">
        <el-form-item label="报销日期" prop="reimburseDate">
          <el-date-picker
            v-model="form.reimburseDate"
            type="date"
            size="small"
            class="date-item filter-item"
            placeholder="选择报销日期"
            format="YYYY-MM-DD"
            value-format="x"
            style="width: 270px"
            :disabled-date="disabledDate"
          />
        </el-form-item>
        <el-form-item label="项目" prop="projectId">
          <project-cascader
            v-model="form.projectId"
            clearable
            :disabled="!form.expenseTypeId || form.costAscriptionEnum === costAscriptionEnum.INDIRECT_COSTS.V"
            class="filter-item"
            style="width: 270px"
            placeholder="请先选择费用类别"
          />
        </el-form-item>
        <el-form-item label="报销人" prop="reimburseUserId">
          <user-select
            ref="userRef"
            v-model="form.reimburseUserId"
            placeholder="选择报销人"
            size="small"
            clearable
            class="filter-item"
            style="width: 270px"
            defaultValue
          />
        </el-form-item>
        <el-form-item label="收款单位" prop="payee">
          <el-input v-model="form.payee" placeholder="输入收款单位" style="width: 270px" maxlength="50" clearable />
        </el-form-item>
        <el-form-item label="费用类别" prop="expenseTypeId">
          <common-select
            v-model="form.expenseTypeId"
            :options="expenseList"
            type="other"
            :data-structure="{ key: 'id', label: 'name', value: 'id' }"
            class="filter-item"
            clearable
            style="width: 270px"
            placeholder="选择费用类别"
            @change="handleChange"
          />
        </el-form-item>
        <el-form-item label="报销科目" prop="expenseSubjectId">
          <common-select
            v-model="form.expenseSubjectId"
            :options="subjectList"
            type="other"
            :data-structure="{ key: 'id', label: 'label', value: 'id' }"
            size="small"
            clearable
            class="filter-item"
            style="width: 270px"
            placeholder="选择报销科目"
          />
        </el-form-item>
        <el-form-item label="费用归属" prop="costAscriptionEnum">
          <span v-if="form.costAscriptionEnum">{{ costAscriptionEnum.VL?.[form.costAscriptionEnum] }}</span>
        </el-form-item>
        <el-form-item label="报销费用（元）" prop="reimburseAmount">
          <el-input-number
            v-show-thousand
            v-model="form.reimburseAmount"
            style="width: 270px"
            placeholder="输入报销费用"
            controls-position="right"
            :precision="DP.YUAN"
            :min="0"
            :max="9999999999"
          />
        </el-form-item>
        <el-form-item label="备注" prop="remark">
          <el-input
            v-model.trim="form.remark"
            type="textarea"
            :autosize="{ minRows: 2, maxRows: 8 }"
            placeholder="输入备注"
            style="width: 270px"
            :maxlength="300"
            show-word-limit
          />
        </el-form-item>
      </el-form>
    </div>
  </common-dialog>
</template>

<script setup>
import { ref, inject } from 'vue'

import { costAscriptionEnum } from '@enum-ms/config'
import { DP } from '@/settings/config'
import { isBlank } from '@data-type/index'

import { regForm } from '@compos/use-crud'
import userSelect from '@comp-common/user-select'
import projectCascader from '@comp-base/project-cascader.vue'

const expenseList = inject('expenseList')
const subjectList = ref([])

const formRef = ref()
const defaultForm = {
  id: undefined,
  payee: undefined,
  projectId: undefined,
  reimburseDate: undefined,
  reimburseUserId: undefined,
  expenseTypeId: undefined,
  expenseSubjectId: undefined,
  reimburseAmount: undefined,
  costAscriptionEnum: undefined
}

const { crud, form, CRUD } = regForm(defaultForm, formRef)

const validateQuantity = (rule, value, callback) => {
  if (!value) {
    callback(new Error('填写数据必须大于0'))
  }
  callback()
}

const validateProject = (rule, value, callback) => {
  if (form.costAscriptionEnum !== costAscriptionEnum.INDIRECT_COSTS.V) {
    if (isBlank(form.expenseTypeId)) {
      callback(new Error('请先选择费用类别'))
    } else if (isBlank(value)) {
      callback(new Error('请选择项目'))
    }
  }
  callback()
}

const rules = {
  reimburseDate: [{ required: true, message: '请选择报销日期', trigger: 'change' }],
  reimburseUserId: [{ required: true, message: '请选择报销人', trigger: 'change' }],
  expenseTypeId: [{ required: true, message: '请选择费用类别', trigger: 'change' }],
  expenseSubjectId: [{ required: true, message: '请选择报销科目', trigger: 'change' }],
  reimburseAmount: [{ required: true, validator: validateQuantity, trigger: 'blur' }],
  projectId: [{ required: true, validator: validateProject, trigger: 'change' }]
}

function handleChange() {
  const row = expenseList.value.find((v) => v.id === form.expenseTypeId) || {}
  subjectList.value = row?.links || []
  form.costAscriptionEnum = row?.costAscriptionEnum
  if (form.costAscriptionEnum === costAscriptionEnum.INDIRECT_COSTS.V || isBlank(form.expenseTypeId)) {
    form.projectId = undefined
  }
}

// 编辑之前
CRUD.HOOK.beforeToEdit = async () => {
  handleChange()
  form.projectId = form.project?.id
}

// 提交前
CRUD.HOOK.beforeSubmit = async () => {
  const valid = await formRef.value.validate()
  if (!valid) return false
}

// 如果时间选取的时间年份比当前的时间大就被禁用
function disabledDate(time) {
  return time > new Date()
}
</script>
