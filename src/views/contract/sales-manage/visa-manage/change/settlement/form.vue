<template>
  <common-drawer
    append-to-body
    :close-on-click-modal="false"
    :before-close="crud.cancelCU"
    :visible="crud.status.cu > 0"
    :title="`${ isEdit ? '编辑' : '新增' }结算单`"
    :show-close="true"
    size="50%"
  >
    <template #titleRight>
      <common-button :loading="crud.status.cu === CRUD.STATUS.PROCESSING" size="mini" type="primary" @click="crud.submitCU">提 交</common-button>
    </template>
    <template  #content>
      <el-form v-loading="crud.editDetailLoading" :disabled="crud.status.cu === CRUD.STATUS.PROCESSING" ref="formRef" :model="form" :rules="rules" size="small" label-width="100px" class="demo-form">
        <div class="rule-row">
          <el-form-item label="项目" prop="projectId">
            <span v-if="isEdit" class="project-name">{{ projectNameFormatter(form.project) }}</span>
            <project-visa-select
              v-else
              v-model="form.projectId"
              clearable
              filterSettlement
              class="input-underline"
              style="width: 100%;"
              @change="handleProjectChange"
            />
          </el-form-item>
          <el-form-item label="项目经理" prop="projectManagerName">
            <div>{{ projectInfo.projectManagerName }}</div>
          </el-form-item>
        </div>
        <div class="rule-row">
          <el-form-item label="结算日期" prop="settlementDate">
            <el-date-picker
              v-model="form.settlementDate"
              type="date"
              class="input-underline"
              value-format="x"
              style="width: 100%"
              placeholder="请选择结算日期"
            />
          </el-form-item>
          <el-form-item label="申请人" prop="userId">
            <user-dept-cascader
              v-model="form.userId"
              filterable
              :collapse-tags="false"
              clearable
              show-all-levels
              class="input-underline"
              placeholder="请选择申请人"
            />
          </el-form-item>
        </div>
        <div class="rule-row">
          <el-form-item label="签约单位" prop="contractSignBodyName">
            <span>{{ projectInfo.contractSignBodyName }}</span>
          </el-form-item>
          <el-form-item label="发包单位" prop="customerUnit">
            <span>{{ projectInfo.customerUnit }}</span>
          </el-form-item>
        </div>
        <div class="rule-row">
          <el-form-item label="签订日期" prop="signingDate">
            <div v-parse-time="{ val: projectInfo.signingDate, fmt: '{y}-{m}-{d}' }" />
          </el-form-item>
          <el-form-item label="合同额" prop="contractAmount">
            <div><span v-thousand="projectInfo.contractAmount || 0" />（{{ digitUppercase(projectInfo.contractAmount || 0) }}）</div>
          </el-form-item>
        </div>
        <div class="rule-row">
          <el-form-item label="保证金额" prop="marginAmount">
            <div><span v-thousand="projectInfo.marginAmount || 0" /><span v-if="projectInfo.marginType">（{{ dict.label['margin_type'][projectInfo.marginType] }}）</span></div>
          </el-form-item>
          <el-form-item label="发运额" prop="happenedAmount">
            <div><span v-thousand="projectInfo.happenedAmount" />（{{ digitUppercase(projectInfo.happenedAmount || 0) }}）</div>
          </el-form-item>
        </div>
        <div class="rule-row">
          <el-form-item label="违约金额" prop="breachAmount">
            <el-input-number
              v-model.number="form.breachAmount"
              :min="0"
              :max="99999999999"
              :step="1000"
              :precision="DP.YUAN"
              class="input-underline"
              placeholder="请输入违约金额"
              :controls="false"
              style="width: 100%"
            />
          </el-form-item>
          <el-form-item label="签证额" prop="visaAmount">
            <el-input-number
              v-model.number="form.visaAmount"
              :min="0"
              :max="99999999999"
              :step="1000"
              :precision="DP.YUAN"
              class="input-underline"
              placeholder="请输入签证额"
              :controls="false"
              style="width: 100%"
            />
          </el-form-item>
        </div>
        <div class="rule-row">
          <el-form-item label="加工结算额" prop="processingSettlementAmount">
            <el-input-number
              v-model.number="form.processingSettlementAmount"
              :min="0"
              :max="99999999999"
              :step="1000"
              :precision="DP.YUAN"
              class="input-underline"
              placeholder="请输入加工结算额"
              :controls="false"
              style="width: 100%"
            />
          </el-form-item>
          <el-form-item label="最终结算额" prop="settlementAmount">
            <el-input-number
              v-model.number="form.settlementAmount"
              :min="0"
              :max="99999999999"
              :step="1000"
              :precision="DP.YUAN"
              class="input-underline"
              placeholder="请输入最终结算额"
              :controls="false"
              style="width: 100%"
            />
          </el-form-item>
        </div>
        <div class="rule-row">
          <el-form-item label="已付款" prop="collectionAmount">
            <div><span v-thousand="projectInfo.collectionAmount || 0" />（{{ digitUppercase(projectInfo.contractAmount || 0) }}）</div>
          </el-form-item>
          <el-form-item label="欠款额" prop="debitAmount">
            <div><span v-thousand="debitAmount" />（{{ digitUppercase(debitAmount || 0) }}）</div>
          </el-form-item>
        </div>
        <div class="rule-row">
          <el-form-item label="已开票" prop="invoiceAmount">
            <div><span v-thousand="projectInfo.invoiceAmount || 0" />（{{ digitUppercase(projectInfo.invoiceAmount || 0) }}）</div>
          </el-form-item>
          <el-form-item label="应补发票" prop="debitInvoice">
            <div><span v-thousand="debitInvoice" />（{{ digitUppercase(debitInvoice || 0) }}）</div>
          </el-form-item>
        </div>
        <div class="rule-row">
          <el-form-item label="备注" prop="remark">
            <el-input
              v-model="form.remark"
              type="textarea"
              :autosize="{ minRows: 2, maxRows: 4 }"
              placeholder="请填写备注"
              maxlength="200"
              show-word-limit
            />
          </el-form-item>
        </div>
      </el-form>
    </template>
  </common-drawer>
</template>

<script setup>
import { getProjectInfo } from '@/api/contract/sales-manage/visa-change'
import { ref, computed } from 'vue'
import { useStore } from 'vuex'

import { DP } from '@/settings/config'
import { digitUppercase } from '@data-type/number'
import { projectNameFormatter } from '@/utils/project'
import { businessTypeEnum } from '@enum-ms/contract'

import { regForm } from '@compos/use-crud'
import useDict from '@compos/store/use-dict'
import userDeptCascader from '@comp-base/user-dept-cascader.vue'
import projectVisaSelect from '@comp-base/project-visa-select.vue'

// 是否是编辑状态
const isEdit = computed(() => {
  return crud.status.edit > 0
})

// 欠款额
const debitAmount = computed(() => {
  return (form.settlementAmount || 0) - (projectInfo.value.collectionAmount || 0)
})

// 应补发票
const debitInvoice = computed(() => {
  return (form.settlementAmount || 0) - (projectInfo.value.invoiceAmount || 0)
})

const formRef = ref()
const projectInfo = ref({})

const store = useStore()

const defaultForm = {
  projectId: undefined,
  visaAmount: undefined,
  settlementDate: undefined,
  settlementAmount: undefined,
  processingSettlementAmount: undefined,
  breachAmount: undefined,
  remark: ''
}

const dict = useDict(['margin_type'])
const { crud, form, CRUD } = regForm(defaultForm, formRef)

const rules = {
  projectId: [{ required: true, message: '请选择项目', trigger: 'change' }],
  settlementDate: [{ required: true, message: '请选择结算日期', trigger: 'change' }],
  userId: [{ required: true, message: '请选择申请人', trigger: 'change' }],
  breachAmount: [{ required: true, message: '请输入违约金', trigger: 'blur' }],
  settlementAmount: [{ required: true, message: '请输入最终结算额', trigger: 'blur' }],
  processingSettlementAmount: [{ required: true, message: '请输入加工结算额', trigger: 'blur' }],
  visaAmount: [{ required: true, message: '请输入签证额', trigger: 'blur' }]
}

// 编辑
CRUD.HOOK.beforeEditDetailLoaded = async (crud) => {
  form.projectId = form.project.id
  handleProjectChange(form.projectId)
}

// 添加成功后更新可签证项目列表
CRUD.HOOK.afterAddSuccess = async () => {
  store.dispatch('project/fetchUserVisaProjects', { businessType: businessTypeEnum.MACHINING.V })
}

// 获取项目详情
async function handleProjectChange(id) {
  projectInfo.value = {}
  try {
    if (id) {
      projectInfo.value = await getProjectInfo(id)
      form.visaAmount = projectInfo.value.visaAmount
    }
  } catch (error) {
    crud.notify('获取项目详情失败', CRUD.NOTIFICATION_TYPE.ERROR)
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
.demo-form .el-form-item {
  flex: 1;
  width: 50%;
  margin-right: 10px;
}
</style>
