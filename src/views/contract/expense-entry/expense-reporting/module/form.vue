<template>
  <common-dialog
    append-to-body
    :close-on-click-modal="false"
    :before-close="crud.cancelCU"
    :visible="crud.status.cu > 0"
    :title="crud.status.title"
    :show-close="false"
    width="30%"
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
      <el-form ref="formRef" :model="form" :rules="rules" size="small" label-width="140px" class="demo-form">
        <el-form-item label="报销日期：" prop="reimburseDate">
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
        <el-form-item label="项目：" prop="projectId">
          <project-cascader v-model="form.projectId" clearable class="filter-item" style="width: 270px" placeholder="选择项目 非必选" />
        </el-form-item>
        <el-form-item label="报销人：" prop="reimburseUserId">
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
        <el-form-item label="费用类别：" prop="expenseTypeId">
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
        <el-form-item label="报销科目：" prop="expenseSubjectId">
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
        <el-form-item label="报销费用（元）：" prop="reimburseAmount">
          <el-input-number
            v-show-thousand
            v-model="form.reimburseAmount"
            style="width: 270px"
            placeholder="输入报销费用"
            controls-position="right"
            :min="0"
            :max="9999999999"
          />
        </el-form-item>
      </el-form>
    </div>
  </common-dialog>
</template>

<script setup>
import { ref, inject } from 'vue'
import { regForm } from '@compos/use-crud'
// import useDict from '@compos/store/use-dict'
import userSelect from '@comp-common/user-select'
import projectCascader from '@comp-base/project-cascader.vue'

const expenseList = inject('expenseList')
const subjectList = ref([])

const formRef = ref()
// const dict = useDict(['reimbursement_type'])
const defaultForm = {}

const { crud, form, CRUD } = regForm(defaultForm, formRef)

const rules = {
  reimburseDate: [{ required: true, message: '请选择报销日期', trigger: 'blur' }],
  reimburseUserId: [{ required: true, message: '请选择报销人', trigger: 'blur' }],
  expenseTypeId: [{ required: true, message: '请选择费用类别', trigger: 'blur' }],
  expenseSubjectId: [{ required: true, message: '请选择报销科目', trigger: 'blur' }],
  reimburseAmount: [{ required: true, message: '请输入报销金额', trigger: 'blur' }]
}

// 刷新数据
CRUD.HOOK.beforeToQuery = (crud, form) => {
}
// 新增之前
CRUD.HOOK.beforeToAdd = (crud, form) => {}

// 编辑之后
CRUD.HOOK.afterToEdit = (crud, form) => {
  console.log(form)
}

// 编辑之前
CRUD.HOOK.beforeToEdit = () => {
  form.projectId = form.project?.id
}

// 提交前
CRUD.HOOK.beforeSubmit = async () => {
}

// 如果时间选取的时间年份比当前的时间大就被禁用
function disabledDate(time) {
  return time > new Date()
}

function handleChange(val) {
  subjectList.value = expenseList.find(v => v.id === val)?.links
}
</script>

<style lang="scss" scoped></style>
