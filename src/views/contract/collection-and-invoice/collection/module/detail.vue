<template>
  <common-drawer
    append-to-body
    v-model="visible"
    top="10vh"
    width="600px"
    :before-close="closeDrawer"
    title="收款信息"
    :wrapper-closable="false"
    size="860px"
  >
    <template #title>
      <div class="dialog-title">
        <span style="margin-right: 5px">收款信息</span>
        <common-button
          v-if="collectionInfo.auditStatus"
          size="mini"
          :type="
            collectionInfo.auditStatus == auditTypeEnum.ENUM.REJECT.V
              ? 'info'
              : collectionInfo.auditStatus == auditTypeEnum.ENUM.PASS.V
              ? 'success'
              : 'warning'
          "
        >
          {{
            collectionInfo.auditStatus == auditTypeEnum.ENUM.REJECT.V
              ? '已驳回'
              : collectionInfo.auditStatus == auditTypeEnum.ENUM.PASS.V
              ? '已通过'
              : '审核中'
          }}
        </common-button>
        <span style="position: absolute; right: 20px">
          <template v-if="collectionInfo.auditStatus">
            <template v-if="!isModify">
              <common-button
                v-if="collectionInfo.auditStatus == auditTypeEnum.ENUM.AUDITING.V && type === 'audit'"
                size="small"
                type="info"
                @click="onSubmit(auditTypeEnum.ENUM.REJECT.V)"
                >驳回</common-button
              >
              <common-button
                v-if="collectionInfo.auditStatus == auditTypeEnum.ENUM.AUDITING.V && type === 'audit'"
                size="small"
                type="success"
                @click="onSubmit(auditTypeEnum.ENUM.PASS.V)"
                >通过</common-button
              >
              <common-button
                size="small"
                type="primary"
                @click="modifyInfo"
                v-if="collectionInfo.auditStatus == auditTypeEnum.ENUM.REJECT.V && type === 'detail'"
                >重新编辑</common-button
              >
            </template>
            <template v-else>
              <common-button type="primary" size="small" @click="onSubmit">提交</common-button>
            </template>
          </template>
          <common-button size="small" @click="closeDrawer">关闭</common-button>
        </span>
      </div>
    </template>
    <template #content>
      <el-form ref="formRef" :model="form" :rules="rules" size="small" label-width="140px">
        <div class="form-row" style="display: flex">
          <el-form-item label="项目" prop="projectId">
            <div style="width: 260px">
              <project-cascader
                v-if="isModify"
                v-model="form.projectId"
                style="width: 250px"
                class="filter-item"
                @change="getContractInfo(form.projectId)"
              />
              <span v-else>{{ collectionInfo.project.serialNumber + ' ' + collectionInfo.project.shortName }}</span>
            </div>
          </el-form-item>
          <el-form-item label="收款日期" prop="collectionDate">
            <div style="width: 260px">
              <el-date-picker
                v-if="isModify"
                v-model="form.collectionDate"
                type="date"
                value-format="x"
                placeholder="选择收款日期"
                style="width: 250px"
              />
              <template v-else>
                <span v-parse-time="'{y}-{m}-{d}'">{{ collectionInfo.collectionDate }}</span>
              </template>
            </div>
          </el-form-item>
        </div>
        <div class="form-row" style="display: flex">
          <el-form-item label="合同金额(元)" prop="contractAmount">
            <div style="width: 260px">
              <el-input
                v-if="isModify"
                v-model="contractInfo.contractAmount"
                type="text"
                placeholder="合同金额"
                style="width: 250px"
                disabled
              />
              <span v-else>{{ collectionInfo.contractAmount ? collectionInfo.contractAmount.toThousand() : '' }}</span>
            </div>
          </el-form-item>
          <el-form-item label="收款单位" prop="collectionUnitId">
            <div style="width: 260px">
              <common-select
                v-if="isModify"
                v-model="form.collectionUnitId"
                :options="contractInfo.companyBankAccountList"
                :type="'other'"
                :dataStructure="typeProp"
                size="small"
                clearable
                class="filter-item"
                placeholder="收款单位"
                style="width: 250px"
                @change="collectionCompanyChange"
              />
              <span v-else>{{ collectionInfo.collectionUnit }}</span>
            </div>
          </el-form-item>
        </div>
        <div class="form-row" style="display: flex">
          <el-form-item label="已收款额(元)">
            <div style="width: 260px">
              <el-input
                v-if="isModify"
                v-model="contractInfo.haveCollectionAmount"
                type="text"
                placeholder="已收款额"
                style="width: 250px"
                disabled
              />
              <span v-else>{{ collectionInfo.haveCollectionAmount ? collectionInfo.haveCollectionAmount.toThousand() : '' }}</span>
            </div>
          </el-form-item>
          <el-form-item label="收款行" prop="collectionDepositBank">
            <div style="width: 260px">
              <el-input v-if="isModify" v-model="form.collectionDepositBank" type="text" placeholder="收款行" style="width: 250px" />
              <span v-else>{{ collectionInfo.collectionDepositBank }}</span>
            </div>
          </el-form-item>
        </div>
        <div class="form-row" style="display: flex">
          <el-form-item label="本次收款金额(元)" prop="collectionAmount">
            <div style="width: 260px">
              <el-input-number
                v-if="isModify"
                v-model.number="form.collectionAmount"
                :min="-99999999999"
                :max="99999999999"
                :step="10000"
                :precision="DP.YUAN"
                placeholder="本次收款金额(元)"
                controls-position="right"
                style="width: 250px"
              />
              <span v-else>{{ collectionInfo.collectionAmount ? collectionInfo.collectionAmount.toThousand() : '' }}</span>
            </div>
          </el-form-item>
          <el-form-item label="收款账号" prop="collectionBankAccount">
            <div style="width: 260px">
              <el-input v-if="isModify" v-model="form.collectionBankAccount" type="text" placeholder="收款账号" style="width: 250px" />
              <span v-else>{{ collectionInfo.collectionBankAccount }}</span>
            </div>
          </el-form-item>
        </div>
        <div class="form-row" style="display: flex">
          <el-form-item label="收款金额大写" prop="paymentAmount1">
            <div style="width: 260px">
              <el-input v-if="isModify" v-model="upperYuan" placeholder="收款金额大写" style="width: 250px" disabled />
              <span v-else>{{ upperYuan }}</span>
            </div>
          </el-form-item>
          <el-form-item label="付款单位" prop="paymentUnit">
            <div style="width: 260px">
              <el-input v-if="isModify" v-model="form.paymentUnit" type="text" placeholder="付款单位" style="width: 250px" />
              <span v-else>{{ collectionInfo.paymentUnit }}</span>
            </div>
          </el-form-item>
        </div>
        <div class="form-row" style="display: flex">
          <el-form-item label="收款事由" prop="collectionReason">
            <div style="width: 260px">
              <common-select
                v-if="isModify"
                v-model="form.collectionReason"
                :options="dict.payment_reason"
                type="dict"
                size="small"
                clearable
                placeholder="收款事由"
                style="width: 250px"
              />
              <span v-else>{{
                collectionInfo.collectionReason && dict && dict.label && dict.label['payment_reason']
                  ? dict.label['payment_reason'][collectionInfo.collectionReason]
                  : ''
              }}</span>
            </div>
          </el-form-item>
          <el-form-item label="付款行" prop="paymentDepositBank">
            <div style="width: 260px">
              <el-input v-if="isModify" v-model="form.paymentDepositBank" type="text" placeholder="付款行" style="width: 250px" />
              <span v-else>{{ collectionInfo.paymentDepositBank }}</span>
            </div>
          </el-form-item>
        </div>
        <div class="form-row" style="display: flex">
          <el-form-item label="收款方式" prop="collectionMode">
            <template #label>
              收款方式
              <el-tooltip effect="light" :content="`选择承兑汇票可在页面下方添加承兑汇票信息`" placement="top">
                <i class="el-icon-info" />
              </el-tooltip>
            </template>
            <div style="width: 260px">
              <common-select
                v-if="isModify"
                v-model="form.collectionMode"
                :options="paymentFineModeEnum.ENUM"
                type="enum"
                size="small"
                placeholder="收款方式"
                style="width: 250px"
              />
              <span v-else>{{ collectionInfo.collectionMode ? paymentFineModeEnum.VL[collectionInfo.collectionMode] : '' }}</span>
            </div>
          </el-form-item>
          <el-form-item label="付款账号" prop="paymentBankAccount">
            <div style="width: 260px">
              <el-input v-if="isModify" v-model="form.paymentBankAccount" type="text" placeholder="付款账号" style="width: 250px" />
              <span v-else>{{ collectionInfo.paymentBankAccount }}</span>
            </div>
          </el-form-item>
        </div>
        <el-collapse-transition>
          <div v-show="form.collectionMode === paymentFineModeEnum.ENUM.ACCEPTANCE_DRAFT.V" class="table-box">
            <common-table ref="table" :data="form.acceptanceDrafts" :cell-class-name="handelCellClassName" style="width: 100%">
              <el-table-column label="序号" type="index" align="center" width="60" />
              <el-table-column prop="amount" label="承兑面额（元）" align="center" min-width="160">
                <template v-slot="scope">
                  <el-input-number
                    v-model="scope.row.amount"
                    :min="1"
                    :max="99999999999"
                    :step="10000"
                    :precision="DP.YUAN"
                    size="small"
                    controls-position="right"
                    placeholder="承兑面额（元）"
                    style="width: 100%; max-width: 200px"
                  />
                </template>
              </el-table-column>
              <el-table-column prop="discount" label="贴现利息（元）" align="center" min-width="160">
                <template v-slot="scope">
                  <el-input-number
                    v-model="scope.row.discount"
                    :max="scope.row.amount || 0"
                    :step="10000"
                    :precision="DP.YUAN"
                    controls-position="right"
                    placeholder="贴现利息（元）"
                    size="small"
                    style="width: 100%; max-width: 200px"
                  />
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
              <el-tooltip
                effect="light"
                :content="`1.贴现利息不可超过承兑面额\n
                2.承兑汇票总金额不可超过付款金额\n`"
                placement="right"
              >
                <div style="display: inline-block">
                  <el-tag type="info">承兑汇票添加规则</el-tag>
                </div>
              </el-tooltip>
            </div>
            <hr class="gradient-line" />
          </div>
        </el-collapse-transition>
        <el-form-item label="备注" prop="remark">
          <el-input
            v-if="isModify"
            v-model="form.remark"
            type="textarea"
            :autosize="{ minRows: 6, maxRows: 8 }"
            placeholder="可填写备注"
            style="max-width: 500px"
          />
          <span v-else>{{ collectionInfo.remark }}</span>
        </el-form-item>
      </el-form>
    </template>
  </common-drawer>
</template>

<script setup>
import { ref, watch, computed, defineProps, defineEmits } from 'vue'
import projectCascader from '@comp-base/project-cascader'
import useDict from '@compos/store/use-dict'
import { DP } from '@/settings/config'
import { paymentFineModeEnum } from '@enum-ms/finance'
import { contractCollectionInfo } from '@/api/contract/collection-and-invoice/collection'
import useWatchFormValidate from '@compos/form/use-watch-form-validate'
import { digitUppercase } from '@/utils/data-type/number'
import useVisible from '@compos/use-visible'
import { auditTypeEnum } from '@enum-ms/contract'
import { editStatus, edit } from '@/api/contract/collection-and-invoice/collection'
import { ElNotification } from 'element-plus'

const formRef = ref()
const dict = useDict(['payment_reason'])
const typeProp = { key: 'companyId', label: 'companyName', value: 'companyId' }
const defaultForm = {
  id: undefined,
  collectionAmount: undefined,
  collectionBankAccount: undefined,
  collectionDate: undefined,
  collectionDepositBank: undefined,
  collectionReason: undefined,
  collectionUnit: undefined,
  collectionUnitId: undefined,
  paymentBankAccount: undefined,
  paymentDepositBank: undefined,
  paymentUnit: undefined,
  projectId: undefined,
  remark: undefined
}

const props = defineProps({
  collectionInfo: {
    type: Object,
    default: () => {}
  },
  modelValue: {
    type: Boolean,
    require: true
  },
  type: {
    type: String,
    require: true
  }
})

const form = ref(JSON.parse(JSON.stringify(defaultForm)))
const contractInfo = ref({})
const isModify = ref(false)
const emit = defineEmits(['success', 'update:modelValue'])
const { visible, handleClose } = useVisible({ emit, props })

watch(
  () => props.collectionInfo.projectId,
  (val) => {
    if (val) {
      getContractInfoReset(val)
    }
  },
  { deep: true, immediate: true }
)

const rules = {
  projectId: [{ required: true, message: '请选择项目', trigger: 'change' }],
  collectionAmount: [{ required: true, message: '请输入本次收款金额', trigger: 'change', type: 'number' }],
  collectionReason: [{ required: true, message: '请选择收款事由', trigger: 'change' }],
  collectionMode: [{ required: true, message: '请选择收款方式', trigger: 'change' }],
  collectionDate: [{ required: true, message: '请选择收款日期', trigger: 'change' }],
  collectionUnitId: [{ required: true, message: '请选择收款单位', trigger: 'change' }],
  paymentUnit: [{ required: true, message: '请输入付款单位', trigger: 'blur' }]
}
const upperYuan = computed(() => {
  if (isModify.value) {
    return form.value.collectionAmount ? digitUppercase(form.value.collectionAmount) : ''
  } else {
    return props.collectionInfo.collectionAmount ? digitUppercase(props.collectionInfo.collectionAmount) : ''
  }
})

function modifyInfo() {
  isModify.value = true
  resetForm()
}

function closeDrawer() {
  isModify.value = false
  handleClose()
}

function resetForm() {
  if (formRef.value) {
    formRef.value.resetFields()
  }
  const DataValue = JSON.parse(JSON.stringify(props.collectionInfo))
  DataValue.collectionDate = String(DataValue.collectionDate)
  DataValue.projectId = DataValue.project.id
  DataValue.collectionUnitId = Number(DataValue.collectionUnitId)
  form.value = JSON.parse(JSON.stringify(DataValue))
  getContractInfoReset(form.value.projectId)
  useWatchFormValidate(formRef, form)
}

async function getContractInfoReset(id) {
  let data = {}
  try {
    data = await contractCollectionInfo({ projectId: id })
  } catch (e) {
    console.log('获取合同信息', e)
  } finally {
    contractInfo.value = data
  }
}

async function getContractInfo(id) {
  let data = {}
  try {
    data = await contractCollectionInfo({ projectId: id })
  } catch (e) {
    console.log('获取合同信息', e)
  } finally {
    contractInfo.value = data
    form.value.paymentBankAccount = contractInfo.value.customerBankCode
    form.value.paymentDepositBank = contractInfo.value.customerBankName
    form.value.paymentUnit = contractInfo.value.customerUnit
    form.value.collectionUnitId = ''
    collectionCompanyChange(form.value.collectionUnitId)
  }
}

function collectionCompanyChange(val) {
  if (val) {
    const collectionVal = contractInfo.value.companyBankAccountList.find((v) => v.companyId === val)
    form.value.collectionBankAccount = collectionVal.account
    form.value.collectionDepositBank = collectionVal.depositBank
    form.value.collectionUnit = collectionVal.companyName
  } else {
    form.value.collectionBankAccount = ''
    form.value.collectionDepositBank = ''
    form.value.collectionUnit = ''
  }
}

function handleSuccess() {
  ElNotification({ title: '提交成功', type: 'success' })
  emit('success')
  closeDrawer()
}

async function onSubmit(val) {
  try {
    if (props.type === 'detail') {
      await formRef.value.validate()
      await edit(form.value)
      handleSuccess()
    } else {
      await editStatus(props.collectionInfo.id, val)
      handleSuccess()
    }
  } catch (e) {
    console.log('收款修改', e)
  }
}
</script>
<style lang="scss" scoped>
::v-deep(.el-input-number .el-input__inner) {
  text-align: left;
}
</style>
