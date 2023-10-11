<template>
  <common-drawer
    append-to-body
    :close-on-click-modal="false"
    :before-close="crud.cancelCU"
    :visible="crud.status.cu > 0"
    :title="`${isEdit ? '编辑' : '新增'}结算单`"
    :show-close="true"
    size="50%"
  >
    <template #titleRight>
      <common-button
:loading="crud.status.cu === CRUD.STATUS.PROCESSING"
size="mini"
type="primary"
@click="handleSubmit"
        >提 交</common-button
      >
    </template>
    <template #content>
      <el-form
        v-loading="crud.editDetailLoading"
        :disabled="crud.status.cu === CRUD.STATUS.PROCESSING"
        ref="formRef"
        :model="form"
        :rules="rules"
        size="small"
        label-width="100px"
        class="demo-form"
      >
        <div class="rule-row">
          <el-form-item label="项目" prop="projectId">
            <span v-if="isEdit" class="project-name">{{ projectNameFormatter(form.project) }}</span>
            <project-visa-select
              v-else
              v-model="form.projectId"
              clearable
              filterSettlement
              class="input-underline"
              style="width: 100%"
              @change="handleProjectChange"
            />
          </el-form-item>
        </div>
        <div class="rule-row">
          <el-form-item label="合同内容" prop="projectContentName">
            <div>{{ projectInfo.projectContentName }}</div>
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
            <div>{{ projectInfo.contractSignBodyName }}</div>
          </el-form-item>
          <el-form-item label="发包单位" prop="customerUnit">
            <div>{{ projectInfo.customerUnit }}</div>
          </el-form-item>
        </div>
        <div class="rule-row">
          <el-form-item label="签约人" prop="signerName">
            <div>{{ projectInfo.signerName }}</div>
          </el-form-item>
          <el-form-item label="合同含税" prop="isTax">
            <div>
              <span>{{ isTaxContractEnum.V?.[projectInfo.isTax]?.['SL'] }}</span>
              <span
v-if="projectInfo.isTax === isTaxContractEnum.YES.V"
                >【{{ invoiceTypeEnum.VL?.[projectInfo.invoiceType] }} {{ projectInfo.taxRate || 0 }}%】</span
              >
            </div>
          </el-form-item>
        </div>
        <div class="rule-row">
          <el-form-item label="签订日期" prop="signingDate">
            <div v-parse-time="{ val: projectInfo.signingDate, fmt: '{y}-{m}-{d}' }" />
          </el-form-item>
          <el-form-item label="合同额" prop="contractAmount">
            <div>
              <span v-thousand="{ val: projectInfo.contractAmount || 0, dp: decimalPrecision.contract }" />（{{
                digitUppercase(projectInfo.contractAmount || 0)
              }}）
            </div>
          </el-form-item>
        </div>
        <div class="rule-row">
          <el-form-item label="保证金额" prop="marginAmount">
            <div v-if="isBlank(projectInfo.marginAmount)">无</div>
            <div v-else>
              <span v-thousand="{ val: projectInfo.marginAmount || 0, dp: decimalPrecision.contract }" />
              <span v-if="projectInfo.marginType">（{{ dict.label['margin_type'][projectInfo.marginType] }}）</span>
            </div>
          </el-form-item>
          <el-form-item label="累计发运额" prop="happenedAmount">
            <div v-if="isBlank(projectInfo.happenedAmount)">无</div>
            <div v-else>
              <span v-thousand="{ val: projectInfo.happenedAmount || 0, dp: decimalPrecision.contract }" />
              （{{ digitUppercase(projectInfo.happenedAmount || 0) }}）
            </div>
          </el-form-item>
        </div>
        <div class="rule-row">
          <el-form-item label="违约金额" prop="breachAmount">
            <el-checkbox v-model="showBreachAmount" size="small" @change="handleBreach">有</el-checkbox>
            <el-input-number
              v-if="showBreachAmount"
              v-model.number="form.breachAmount"
              :min="0.01"
              :max="99999999999"
              :step="1000"
              :precision="decimalPrecision.contract"
              class="input-underline"
              placeholder="请输入违约金额"
              :controls="false"
              style="width: calc(100% - 40px)"
            />
          </el-form-item>
          <el-form-item label="签证额" prop="visaAmount">
            <el-input-number
              v-model.number="form.visaAmount"
              :min="0"
              :max="99999999999"
              :step="1000"
              :precision="decimalPrecision.contract"
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
              :precision="decimalPrecision.contract"
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
              :precision="decimalPrecision.contract"
              class="input-underline"
              placeholder="请输入最终结算额"
              :controls="false"
              style="width: 100%"
            />
          </el-form-item>
        </div>
        <div class="rule-row">
          <el-form-item label="累计收款" prop="collectionAmount">
            <div>
              <span v-thousand="{ val: projectInfo.collectionAmount || 0, dp: decimalPrecision.contract }" />（{{
                digitUppercase(projectInfo.collectionAmount || 0)
              }}）
            </div>
          </el-form-item>
          <el-form-item label="结算应收" prop="receivable">
            <div>
              <span v-thousand="{ val: receivable || 0, dp: decimalPrecision.contract }" />
              （{{ digitUppercase(receivable || 0) }}）
            </div>
          </el-form-item>
        </div>
        <div class="rule-row">
          <el-form-item label="累计开票" prop="invoiceAmount">
            <div>
              <span v-thousand="{ val: projectInfo.invoiceAmount || 0, dp: decimalPrecision.contract }" />（{{
                digitUppercase(projectInfo.invoiceAmount || 0)
              }}）
            </div>
          </el-form-item>
          <el-form-item label="应补发票" prop="debitInvoice">
            <div v-if="projectInfo.isTax === isTaxContractEnum.YES.V">
              <span>{{ debitInvoice ? toThousand(debitInvoice, decimalPrecision.contract) : 0 }}</span>
              （{{ digitUppercase(debitInvoice || 0) }}）
            </div>
            <div v-else>无</div>
          </el-form-item>
        </div>
        <div class="item-center">
          <upload-list
            :accept="'.jpg,.png,.pdf,.jpeg'"
            :file-classify="fileClassifyEnum.CONTRACT_VISA.V"
            showView
            showDownload
            v-model:files="detail.files"
            style="padding-bottom: 20px"
          >
            <template #applicant>
              <el-table-column label="上传人" align="center" min-width="60" :show-overflow-tooltip="true" prop="createUserName">
              </el-table-column>
            </template>
          </upload-list>
          <el-input
            v-model="form.remark"
            type="textarea"
            :autosize="{ minRows: 2, maxRows: 4 }"
            placeholder="请填写备注"
            maxlength="200"
            show-word-limit
          />
        </div>
        <!-- <div class="rule-row">
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
        </div> -->
      </el-form>
    </template>
  </common-drawer>
</template>

<script setup>
import { getProjectInfo } from '@/api/contract/sales-manage/settlement-manage'
import { ref, computed, watch } from 'vue'
import { useStore } from 'vuex'
import { mapGetters } from '@/store/lib'

import moment from 'moment'
import { isBlank } from '@data-type/index'
import { toThousand, digitUppercase } from '@data-type/number'
import { projectNameFormatter } from '@/utils/project'
import { businessTypeEnum, isTaxContractEnum } from '@enum-ms/contract'
import { invoiceTypeEnum } from '@/utils/enum/modules/finance'
import { fileClassifyEnum } from '@enum-ms/file'
import { reviewStatusEnum } from '@enum-ms/common'

import { regForm } from '@compos/use-crud'
import { regDetail } from '@compos/use-crud'
import uploadList from '@comp/file-upload/UploadList.vue'
import useDict from '@compos/store/use-dict'
import userDeptCascader from '@comp-base/user-dept-cascader.vue'
import projectVisaSelect from '@comp-base/project-visa-select'
import useDecimalPrecision from '@compos/store/use-decimal-precision'

const { decimalPrecision } = useDecimalPrecision()

const { user } = mapGetters('user')

// 是否是编辑状态
const isEdit = computed(() => {
  return crud.status.edit > 0
})

// 应收款
const receivable = computed(() => {
  return (form.settlementAmount || 0) - (projectInfo.value.collectionAmount || 0)
})

// 应补发票
const debitInvoice = computed(() => {
  return (form.settlementAmount || 0) - (projectInfo.value.invoiceAmount || 0)
})

const formRef = ref()
const projectInfo = ref({})
const showBreachAmount = ref(false)

const store = useStore()

const defaultForm = {
  projectId: undefined,
  visaAmount: undefined,
  settlementDate: moment().startOf('day').format('x'),
  settlementAmount: undefined,
  processingSettlementAmount: undefined,
  breachAmount: undefined,
  remark: '',
  userId: user?.value?.id
}

const dict = useDict(['margin_type'])
const { crud, form, CRUD } = regForm(defaultForm, formRef)
const { detail } = regDetail()

watch([() => form?.breachAmount, () => form?.processingSettlementAmount, () => form?.visaAmount], () => {
  form.settlementAmount = (form.processingSettlementAmount || 0) + (form.visaAmount || 0) - (form.breachAmount || 0)
})

const rules = {
  projectId: [{ required: true, message: '请选择项目', trigger: 'change' }],
  settlementDate: [{ required: true, message: '请选择结算日期', trigger: 'change' }],
  userId: [{ required: true, message: '请选择申请人', trigger: 'change' }],
  breachAmount: [{ required: false, message: '请输入违约金', trigger: 'blur' }],
  settlementAmount: [{ required: true, message: '请输入最终结算额', trigger: 'blur' }],
  processingSettlementAmount: [{ required: true, message: '请输入加工结算额', trigger: 'blur' }],
  visaAmount: [{ required: true, message: '请输入签证额', trigger: 'blur' }]
}

// CRUD.HOOK.beforeToCU = async (crud) => {
//   console.log(form)
// }

CRUD.HOOK.beforeDetailLoaded = async (crud) => {
  try {
    console.log(detail)
    detail.files = detail.attachments || []
  } catch (error) {
    crud.notify('获取结算单详情失败', CRUD.NOTIFICATION_TYPE.ERROR)
  }
}

// 编辑
CRUD.HOOK.beforeEditDetailLoaded = async (crud) => {
  console.log(form)
  form.projectId = form.project.id
  detail.files = form.attachments || []
  showBreachAmount.value = !!form.breachAmount
  handleProjectChange(form.projectId)
}

// 编辑
CRUD.HOOK.beforeToAdd = async (crud) => {
  showBreachAmount.value = false
  projectInfo.value = {}
}

// 添加成功后更新可签证项目列表
CRUD.HOOK.afterAddSuccess = async () => {
  store.dispatch('project/fetchUserVisaProjects', { businessType: businessTypeEnum.MACHINING.V })
}

// 处理违约金
function handleBreach(val) {
  rules['breachAmount'][0].required = val
  if (!val) {
    form.breachAmount = void 0
  }
}

// 获取项目详情
async function handleProjectChange(id) {
  projectInfo.value = {}
  try {
    if (id) {
      projectInfo.value = await getProjectInfo(id)
      // 新增的时候赋值
      if (!isEdit.value) {
        form.visaAmount = projectInfo.value.visaAmount
      }
    }
  } catch (error) {
    crud.notify('获取项目详情失败', CRUD.NOTIFICATION_TYPE.ERROR)
  }
}

// 表当提交
async function handleSubmit(status) {
  try {
    const params = {
      id: detail.id
    }
    if (status) {
      if (!detail.files.length) {
        crud.notify('请上传确签附件', CRUD.NOTIFICATION_TYPE.WARNING)
        return
      }
      form.attachmentIds = detail.files.map((f) => f.id)
    } else {
      params.status = reviewStatusEnum.REFUSE.V
    }
    crud.submitCU()
  } catch (error) {
    crud.notify('提交结算单确签信息', CRUD.NOTIFICATION_TYPE.ERROR)
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
.item-center {
  margin: 10px;
}
</style>
