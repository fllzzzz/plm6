<template>
  <common-drawer
    append-to-body
    :close-on-click-modal="false"
    :before-close="crud.cancelCU"
    :visible="crud.status.cu > 0"
    :title="`${ isEdit ? '编辑' : '新增' }签证单`"
    :show-close="true"
    size="50%"
  >
    <template #titleRight>
      <common-button :loading="crud.status.cu === CRUD.STATUS.PROCESSING" size="mini" type="primary" @click="crud.submitCU">提 交</common-button>
    </template>
    <template  #content>
      <div class="form">
        <el-form v-loading="crud.editDetailLoading" :disabled="crud.status.cu === CRUD.STATUS.PROCESSING" ref="formRef" :model="form" :rules="rules" size="small" label-width="100px" class="demo-form">
          <div class="rule-row">
            <el-form-item label="项目" prop="projectId">
              <span v-if="isEdit" class="project-name">{{ projectNameFormatter(form.project) }}</span>
              <project-visa-select
                v-else
                v-model="form.projectId"
                clearable
                style="width: 100%;"
                :businessType="businessTypeEnum.INSTALLATION.V"
                @change="handleProjectChange"
              />
            </el-form-item>
            <el-form-item label="" />
          </div>
          <div class="rule-row">
            <el-form-item label="申请日期" prop="visaDate">
              <el-date-picker
                v-model="form.visaDate"
                type="date"
                value-format="x"
                style="width: 100%"
                placeholder="申请日期"
              />
            </el-form-item>
            <el-form-item label="签证人" prop="userId">
              <user-dept-cascader
                v-model="form.userId"
                filterable
                :collapse-tags="false"
                clearable
                show-all-levels
                placeholder="签证人"
              />
            </el-form-item>
          </div>
          <div class="rule-row">
            <el-form-item label="施工单位" prop="contractSignBodyName">
              <span>{{ form.contractSignBodyName }}</span>
            </el-form-item>
            <el-form-item label="发包单位" prop="customerUnit">
              <span>{{ form.customerUnit }}</span>
            </el-form-item>
          </div>
          <div class="rule-row">
            <el-form-item label="签证原因" prop="reasonId">
              <common-select
                v-model="form.reasonId"
                :options="dict.visa_reason"
                type="dict"
                clearable
                placeholder="请选择签证原因"
                style="width:100%;"
                size="medium"
              />
            </el-form-item>
            <el-form-item label="签证额" prop="visaAmount">
              <el-input-number
                v-model.number="form.visaAmount"
                :min="0"
                :max="99999999999"
                :step="1000"
                :precision="decimalPrecision.project"
                placeholder="请输入签证额"
                :controls="false"
                style="width: 100%;text-align: left;"
              />
            </el-form-item>
          </div>
          <div class="rule-row">
            <el-form-item label="说明" prop="remark">
              <el-input
                v-model.trim="form.remark"
                type="textarea"
                :autosize="{ minRows: 2, maxRows: 4 }"
                placeholder="请填写备注"
                maxlength="200"
                show-word-limit
              />
            </el-form-item>
          </div>
           <div class="rule-row">
            <el-form-item label="施工单位" prop="contractSignBodyName">
              <span>{{ form.contractSignBodyName }}</span>
            </el-form-item>
            <el-form-item label="" />
          </div>
          <div class="rule-row">
            <el-form-item label="设计单位" prop="designUnit">
              <el-input v-model.trim="form.designUnit" placeholder="请输入设计单位" maxlength="32" />
            </el-form-item>
            <el-form-item label="" />
          </div>
          <div class="rule-row">
            <el-form-item label="监理单位" prop="supervisionUnit">
              <el-input v-model.trim="form.supervisionUnit" placeholder="请输入监理单位" maxlength="32" />
            </el-form-item>
            <el-form-item label="" />
          </div>
           <div class="rule-row">
            <el-form-item label="发包单位" prop="customerUnit">
              <span>{{ form.customerUnit }}</span>
            </el-form-item>
            <el-form-item label="" />
          </div>
        </el-form>
      </div>
    </template>
  </common-drawer>
</template>

<script setup>
import { getProjectInfo } from '@/api/contract/sales-manage/visa-manage'
import { ref, computed } from 'vue'

import { projectNameFormatter } from '@/utils/project'
import { businessTypeEnum } from '@enum-ms/contract'

import { regForm } from '@compos/use-crud'
import useDict from '@compos/store/use-dict'
import userDeptCascader from '@comp-base/user-dept-cascader.vue'
import projectVisaSelect from '@comp-base/project-visa-select.vue'
import useDecimalPrecision from '@compos/store/use-decimal-precision'

const { decimalPrecision } = useDecimalPrecision()

// 是否是编辑状态
const isEdit = computed(() => {
  return crud.status.edit > 0
})

const formRef = ref()

const defaultForm = {
  projectId: undefined,
  visaAmount: undefined,
  visaDate: undefined,
  reasonId: undefined,
  governmentRegulation: '',
  supervisionUnit: '',
  designUnit: '',
  remark: '',
  userId: ''
  // date: undefined
}

const dict = useDict(['visa_reason'])
const { crud, form, CRUD } = regForm(defaultForm, formRef)

const rules = {
  projectId: [{ required: true, message: '请选择项目', trigger: 'change' }],
  visaDate: [{ required: true, message: '请选择签证日期', trigger: 'change' }],
  userId: [{ required: true, message: '请选择签证人', trigger: 'change' }],
  reasonId: [{ required: true, message: '请选择签证原因', trigger: 'change' }],
  visaAmount: [{ required: true, message: '请输入签证额', trigger: 'blur' }]
}

// 新增
CRUD.HOOK.afterToAdd = () => {
  setTimeout(() => {
    form.projectId = crud.query.projectId
    handleProjectChange(form.projectId)
  }, 300)
}

// 编辑
CRUD.HOOK.beforeEditDetailLoaded = async (crud) => {
  form.projectId = form.project.id
  handleProjectChange(form.projectId)
}

// 获取项目详情
async function handleProjectChange(id) {
  let projectInfo = {}
  try {
    if (id) {
      projectInfo = await getProjectInfo(id)
    }
  } catch (error) {
    crud.notify('获取项目详情失败', CRUD.NOTIFICATION_TYPE.ERROR)
  } finally {
    form.customerUnit = projectInfo.customerUnit
    form.contractSignBodyName = projectInfo.contractSignBodyName
  }
}
</script>

<style rel="stylesheet/scss" lang="scss" scoped>
::v-deep(.el-input-number .el-input__inner) {
  text-align: left;
}
.demo-form .rule-row {
  display: flex;
  margin-bottom: 10px;
}
.demo-form .rule-row:last-child {
  margin-bottom: 0px;
}
.form .el-form-item {
  flex: 1;
  width: 50%;
  margin-right: 10px;
}
</style>
