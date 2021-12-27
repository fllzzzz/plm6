<template>
  <common-drawer
    append-to-body
    v-model="visible"
    top="10vh"
    width="600px"
    :before-close="closeDrawer"
    title="付款信息"
    :wrapper-closable="false"
    size="860px"
  >
    <template #title>
      <div class="dialog-title">
        <span style="margin-right:5px;">付款信息</span>
        <common-button v-if="paymentDetailInfo.auditStatus" size="mini" :type="paymentDetailInfo.auditStatus==auditTypeEnum.ENUM.REJECT.V?'info':(paymentDetailInfo.auditStatus==auditTypeEnum.ENUM.PASS.V?'success':'warning')">
          {{ paymentDetailInfo.auditStatus==auditTypeEnum.ENUM.REJECT.V?'已驳回':(paymentDetailInfo.auditStatus==auditTypeEnum.ENUM.PASS.V?'已通过':'审核中') }}
        </common-button>
        <span style="position:absolute;right:20px;">
          <template v-if="paymentDetailInfo.auditStatus">
            <template v-if="!isModify">
              <common-button v-if="paymentDetailInfo.auditStatus==auditTypeEnum.ENUM.AUDITING.V && type==='audit'" size="small" type="info" @click="onSubmit(auditTypeEnum.ENUM.REJECT.V)">驳回</common-button>
              <common-button v-if="paymentDetailInfo.auditStatus==auditTypeEnum.ENUM.AUDITING.V && type==='audit'" size="small" type="success" @click="onSubmit(auditTypeEnum.ENUM.PASS.V)">通过</common-button>
              <!-- <common-button size="small" type="primary" @click="modifyInfo" v-if="paymentDetailInfo.auditStatus==auditTypeEnum.ENUM.REJECT.V && type==='detail'">重新编辑</common-button> -->
            </template>
            <template v-else>
              <common-button type="primary" size="small" @click="onSubmit">提交</common-button>
            </template>
          </template>
          <common-button size="small"  @click="closeDrawer">关闭</common-button>
        </span>
      </div>
    </template>
    <template #content>
      <el-form ref="formRef" :model="form" :rules="rules" size="small" label-width="140px">
        <div style="display:flex;">
          <el-form-item label="属性" prop="propertyType">
            <div style="width:260px;">
              <common-select
                v-if="isModify"
                v-model="form.propertyType"
                :options="supplierPayMentTypeEnum.ENUM"
                type="enum"
                size="small"
                clearable
                class="filter-item"
                placeholder="属性"
                style="width:250px"
                @change="getOrderInfo"
              />
              <span v-else>{{ paymentDetailInfo.propertyType? supplierPayMentTypeEnum.VL[paymentDetailInfo.propertyType] : '' }}</span>
            </div>
          </el-form-item>
          <el-form-item label="付款事由" prop="paymentReason">
            <div style="width:260px;">
              <common-select
                v-if="isModify"
                v-model="form.paymentReason"
                :options="dict.payment_reason"
                type="dict"
                clearable
                size="small"
                placeholder="付款事由"
                style="width:250px"
              />
              <span v-else>{{ paymentDetailInfo.paymentReason && dict && dict.label && dict.label['payment_reason']? dict.label['payment_reason'][ paymentDetailInfo.paymentReason]: '' }}</span>
            </div>
          </el-form-item>
        </div>
        <div style="display:flex;">
          <el-form-item :label="(form.propertyType===supplierPayMentTypeEnum.ENUM.PRODUCT.V || form.propertyType===supplierPayMentTypeEnum.ENUM.SUBCONTRACT.V)?'所属订单':'订单号'" prop="orderId">
            <div style="width:260px;">
              <common-select
                v-if="isModify"
                v-model="form.orderId"
                :options="orderListOption"
                :type="'other'"
                :dataStructure="orderProp"
                size="small"
                clearable
                class="filter-item"
                placeholder="订单号"
                style="width:250px"
                @change="orderChange"
              />
              <span v-else>{{ paymentDetailInfo.orderSerialNumber }}</span>
            </div>
          </el-form-item>
          <el-form-item label="付款方式" prop="payType">
            <div style="width:260px;">
              <common-select
                v-if="isModify"
                v-model="form.payType"
                :options="supplierPayModeEnum.ENUM"
                type="enum"
                size="small"
                placeholder="付款方式"
                style="width: 250px;"
              />
              <span v-else>{{ paymentDetailInfo.payForType? contractPayForEnum.VL[paymentDetailInfo.payForType]: '' }}</span>
            </div>
          </el-form-item>
        </div>
        <div style="display:flex;">
          <template v-if="isModify">
            <el-form-item v-if="form.propertyType!=supplierPayMentTypeEnum.ENUM.SUBCONTRACT.V" label="费用类别" prop="payForTypeName">
              <div style="width:260px;">
                <el-input
                  v-model="form.payForTypeName"
                  type="text"
                  placeholder="费用类别"
                  style="width: 250px;"
                  disabled
                />
              </div>
            </el-form-item>
            <el-form-item v-if="form.propertyType!=supplierPayMentTypeEnum.ENUM.SUBCONTRACT.V" label="种类" prop="basicClassName">
              <div style="width:260px;">
                <el-input
                  v-model="form.basicClassName"
                  type="text"
                  placeholder="种类"
                  style="width: 250px;"
                  disabled
                />
              </div>
            </el-form-item>
          </template>
          <template v-else>
            <el-form-item v-if="paymentDetailInfo.propertyType!=supplierPayMentTypeEnum.ENUM.SUBCONTRACT.V" label="费用类别" prop="payForTypeName">
              <div style="width:260px;">
                <span>{{ paymentDetailInfo.payForType? contractPayForEnum.VL[ paymentDetailInfo.payForType ]: ''}}</span>
              </div>
            </el-form-item>
            <el-form-item v-if="paymentDetailInfo.propertyType!=supplierPayMentTypeEnum.ENUM.SUBCONTRACT.V" label="种类" prop="basicClassName">
              <div style="width:260px;">
                <span>{{ paymentDetailInfo.basicClassName }}</span>
              </div>
            </el-form-item>
          </template>
        </div>
        <div style="display:flex;">
          <el-form-item label="供应商" prop="supplierName">
            <div style="width:260px;">
              <el-input
                v-if="isModify"
                v-model="form.supplierName"
                type="text"
                placeholder="供应商"
                style="width: 250px;"
                disabled
              />
              <span v-else>{{ paymentDetailInfo.supplierName }}</span>
            </div>
          </el-form-item>
          <el-form-item label="付款日期" prop="paymentDate">
            <div style="width:260px;">
              <el-date-picker
                v-if="isModify"
                v-model="form.paymentDate"
                type="date"
                value-format="x"
                placeholder="选择付款日期"
                style="width: 250px;"
              />
              <template v-else>
                <span v-parse-time="'{y}-{m}-{d}'">{{ paymentDetailInfo.paymentDate }}</span>
              </template>
            </div>
          </el-form-item>
        </div>
        <div style="display:flex;">
          <template v-if="isModify">
            <el-form-item v-if="form.propertyType !=supplierPayMentTypeEnum.ENUM.MATERIAL_TRANSPORT.V && form.propertyType !=supplierPayMentTypeEnum.ENUM.PRODUCT_TRANSPORT.V" label="合同金额(元)" prop="contractAmount">
              <div style="width:260px;">
                <el-input
                  v-model="choseOrderInfo.amount"
                  type="text"
                  placeholder="合同金额(元)"
                  style="width: 250px;"
                  disabled
                />
              </div>
            </el-form-item>
            <el-form-item v-if="form.propertyType===supplierPayMentTypeEnum.ENUM.MATERIAL_TRANSPORT.V || form.propertyType===supplierPayMentTypeEnum.ENUM.PRODUCT_TRANSPORT.V " label="运费金额(元)" prop="contractAmount">
              <div style="width:260px;">
                <el-input
                  v-model="choseOrderInfo.amount"
                  type="text"
                  placeholder="运费金额(元)"
                  style="width: 250px;"
                  disabled
                />
              </div>
            </el-form-item>
          </template>
          <template v-else>
            <el-form-item v-if="paymentDetailInfo.propertyType !=supplierPayMentTypeEnum.ENUM.MATERIAL_TRANSPORT.V && paymentDetailInfo.propertyType !=supplierPayMentTypeEnum.ENUM.PRODUCT_TRANSPORT.V" label="合同金额(元)" prop="contractAmount">
              <div style="width:260px;">
                <span>{{ choseOrderInfo.amount && choseOrderInfo.amount>0? choseOrderInfo.amount.toThousand(): choseOrderInfo.amount }}</span>
              </div>
            </el-form-item>
            <el-form-item v-if="paymentDetailInfo.propertyType===supplierPayMentTypeEnum.ENUM.MATERIAL_TRANSPORT.V || paymentDetailInfo.propertyType===supplierPayMentTypeEnum.ENUM.PRODUCT_TRANSPORT.V " label="运费金额(元)" prop="contractAmount">
              <div style="width:260px;">
                <span>{{ choseOrderInfo.amount && choseOrderInfo.amount>0? choseOrderInfo.amount.toThousand(): choseOrderInfo.amount }}</span>
              </div>
            </el-form-item>
          </template>
          <el-form-item label="付款单位" prop="paymentUnitId">
            <div style="width:260px;">
              <common-select
                v-if="isModify"
                v-model="form.paymentUnitId"
                :options="choseOrderInfo.companyBankAccountList"
                :type="'other'"
                :dataStructure="typeProp"
                size="small"
                clearable
                class="filter-item"
                placeholder="付款单位"
                style="width:250px"
                @change="orderCompanyChange"
              />
              <span v-else>{{ paymentDetailInfo.paymentUnit }}</span>
            </div>
          </el-form-item>
        </div>
        <div style="display:flex;">
          <el-form-item label="入库累计(元)">
            <div style="width:260px;">
              <el-input
                v-if="isModify"
                v-model="choseOrderInfo.inBoundAmount"
                type="text"
                placeholder="入库累计(元)"
                style="width: 250px;"
                disabled
              />
              <span v-else>{{ choseOrderInfo.inBoundAmount && choseOrderInfo.inBoundAmount>0? choseOrderInfo.inBoundAmount.toThousand(): choseOrderInfo.inBoundAmount }}</span>
            </div>
          </el-form-item>
        </div>
        <div style="display:flex;">
           <el-form-item label="申请人" prop="applyUserId">
            <div style="width:260px;">
              <user-dept-cascader
                v-if="isModify"
                v-model="form.applyUserId"
                filterable
                clearable
                show-all-levels
                style="width:250px"
                placeholder="申请人"
              />
              <span v-else>{{ paymentDetailInfo.applyUserName }}</span>
            </div>
          </el-form-item>
          <el-form-item label="付款行" prop="paymentBank">
            <div style="width:260px;">
              <el-input
                v-if="isModify"
                v-model="form.paymentBank"
                type="text"
                placeholder="付款行"
                style="width: 250px;"
              />
              <span v-else>{{ paymentDetailInfo.paymentBank }}</span>
            </div>
          </el-form-item>
        </div>
        <div style="display:flex;">
           <el-form-item label="申请日期" prop="applyDate">
            <div style="width:260px;">
              <el-date-picker
                v-if="isModify"
                v-model="form.applyDate"
                type="date"
                value-format="x"
                placeholder="选择申请日期"
                style="width: 250px;"
              />
              <template v-else>
                <span v-parse-time="'{y}-{m}-{d}'">{{ paymentDetailInfo.applyDate }}</span>
              </template>
            </div>
          </el-form-item>
          <el-form-item label="付款账号" prop="paymentBankAccount">
            <div style="width:260px;">
              <el-input
                v-if="isModify"
                v-model="form.paymentBankAccount"
                type="text"
                placeholder="付款账号"
                style="width: 250px;"
              />
              <span v-else>{{ paymentDetailInfo.paymentBankAccount }}</span>
            </div>
          </el-form-item>
        </div>
        <div style="display:flex;">
          <el-form-item label="申请金额(元)" prop="applyAmount">
            <div style="width:260px;">
              <el-input-number
                v-if="isModify"
                v-model.number="form.applyAmount"
                :min="0"
                :max="choseOrderInfo.amount? choseOrderInfo.amount : 99999999999"
                :step="10000"
                :precision="DP.YUAN"
                placeholder="申请金额(元)"
                controls-position="right"
                style="width: 250px;"
              />
              <span v-else>{{ paymentDetailInfo.applyAmount && paymentDetailInfo.applyAmount>0? paymentDetailInfo.applyAmount.toThousand(): paymentDetailInfo.applyAmount }}</span>
            </div>
          </el-form-item>
          <el-form-item label="收款单位" prop="receiveUnit">
            <div style="width:260px;">
              <el-input
                v-if="isModify"
                v-model="form.receiveUnit"
                type="text"
                placeholder="收款方"
                style="width: 250px;"
              />
              <span v-else>{{ paymentDetailInfo.receiveUnit }}</span>
            </div>
          </el-form-item>
        </div>
        <div style="display:flex;">
          <el-form-item label="付款金额(元)" prop="paymentAmount">
            <div style="width:260px;">
              <el-input-number
                v-if="isModify"
                v-model.number="form.paymentAmount"
                :min="0"
                :max="choseOrderInfo.amount? choseOrderInfo.amount : 99999999999"
                :step="10000"
                :precision="DP.YUAN"
                placeholder="付款金额(元)"
                controls-position="right"
                style="width: 250px;"
              />
              <div v-else>
                <span>{{ paymentDetailInfo.paymentAmount && paymentDetailInfo.paymentAmount>0? paymentDetailInfo.paymentAmount.toThousand(): paymentDetailInfo.paymentAmount }}</span>
                <span>{{ upperYuan }}</span>
              </div>
            </div>
            <div style="width:260px;">
              <el-input
                v-if="isModify"
                v-model="upperYuan"
                type="text"
                placeholder="付款金额大写"
                style="width: 250px;"
                disabled
              />
            </div>
          </el-form-item>
          <el-form-item label="收款行" prop="paymentBank">
            <div style="width:260px;">
              <el-input
                v-if="isModify"
                v-model="form.paymentBank"
                type="text"
                placeholder="收款方"
                style="width: 250px;"
              />
              <span v-else>{{ paymentDetailInfo.paymentBank }}</span>
            </div>
          </el-form-item>
        </div>
        <div style="display:flex;">
          <el-form-item label="付款比例" prop="paymentRate">
            <div style="width:260px;">
              <el-input
                v-if="isModify"
                v-model="paymentRate"
                type="text"
                placeholder="付款比例"
                style="width: 250px;"
                disabled
              />
              <span v-else>{{ paymentRate }}</span>
            </div>
          </el-form-item>
          <el-form-item label="收款账号" prop="receiveBankAccount">
            <div style="width:260px;">
              <el-input
                v-if="isModify"
                v-model="form.receiveBankAccount"
                type="text"
                placeholder="收款账号"
                style="width: 250px;"
              />
              <span v-else>{{ paymentDetailInfo.receiveBankAccount }}</span>
            </div>
          </el-form-item>
        </div>
        <div style="display:flex;">
          <template v-if="isModify">
            <el-form-item v-if="form.propertyType===supplierPayMentTypeEnum.ENUM.PRODUCT.V || form.propertyType===supplierPayMentTypeEnum.ENUM.SUBCONTRACT.V" label="关联项目" prop="projectId">
              <div style="width:260px;">
                <template v-if="choseOrderInfo.projectList && choseOrderInfo.projectList.length>0">
                  <div v-for="item in choseOrderInfo.projectList" :key="item.id">{{item.serialNumber+' '+item.shortName}}</div>
                </template>
              </div>
            </el-form-item>
          </template>
          <template v-else>
            <el-form-item v-if="paymentDetailInfo.propertyType===supplierPayMentTypeEnum.ENUM.PRODUCT.V || paymentDetailInfo.propertyType===supplierPayMentTypeEnum.ENUM.SUBCONTRACT.V" label="关联项目" prop="projectId">
              <div style="width:260px;">
                <template v-if="choseOrderInfo.projectList && choseOrderInfo.projectList.length>0">
                  <div v-for="item in choseOrderInfo.projectList" :key="item.id">{{item.serialNumber+' '+item.shortName}}</div>
                </template>
              </div>
            </el-form-item>
          </template>
          <el-form-item label="附件">
            <div style="width:260px;">
              <upload-btn ref="uploadRef" v-model:files="form.attachments" :file-classify="fileClassifyEnum.ENUM.NORMAL.V" v-if="isModify" />
              <template v-else>
                <div v-for="item in paymentDetailInfo.attachmentList" :key="item.id">{{ item.name }}</div>
              </template>
            </div>
          </el-form-item>
        </div>
        <el-form-item label="备注" prop="remark">
          <el-input
            v-if="isModify"
            v-model="form.remark"
            type="textarea"
            :autosize="{ minRows: 6, maxRows: 8}"
            placeholder="可填写备注"
            style="max-width: 500px;"
          />
          <span v-else>{{ paymentDetailInfo.remark }}</span>
        </el-form-item>
      </el-form>
    </template>
  </common-drawer>
</template>

<script setup>
import { ref, watch, computed, defineProps, defineEmits } from 'vue'
import useDict from '@compos/store/use-dict'
import { DP } from '@/settings/config'
import { orderInfo, editStatus } from '@/api/contract/supplier-manage/pay-invoice/pay'
// import useWatchFormValidate from '@compos/form/use-watch-form-validate'
import { digitUppercase } from '@/utils/data-type/number'
import useVisible from '@compos/use-visible'
import { supplierPayMentTypeEnum, contractPayForEnum, supplierPayModeEnum, auditTypeEnum } from '@enum-ms/contract'
import { ElNotification } from 'element-plus'
import { fileClassifyEnum } from '@enum-ms/file'
import UploadBtn from '@comp/file-upload/UploadBtn'
import userDeptCascader from '@comp-base/user-dept-cascader.vue'

const formRef = ref()
const dict = useDict(['payment_reason'])
const orderProp = { key: 'id', label: 'serialNumber', value: 'id' }
const typeProp = { key: 'companyId', label: 'companyName', value: 'companyId' }
const defaultForm = {
  id: undefined,
  applyAmount: undefined,
  applyDate: undefined,
  applyUserId: undefined,
  applyUserName: undefined,
  attachments: undefined,
  attachmentIds: undefined,
  basicClass: undefined,
  basicClassName: undefined,
  orderId: undefined,
  orderSerialNumber: undefined,
  payForType: undefined,
  payForTypeName: undefined,
  payType: undefined,
  paymentAmount: undefined,
  paymentBank: undefined,
  paymentBankAccount: undefined,
  paymentDate: undefined,
  paymentReason: undefined,
  paymentUnit: undefined,
  propertyType: undefined,
  receiveBank: undefined,
  receiveBankAccount: undefined,
  receiveUnit: undefined,
  remark: undefined,
  source: undefined,
  supplierId: undefined,
  supplierName: undefined
}

const props = defineProps({
  paymentDetailInfo: {
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
const isModify = ref(false)
const emit = defineEmits(['success', 'update:modelValue'])
const { visible, handleClose } = useVisible({ emit, props })

const choseOrderInfo = ref({})
const orderListOption = ref([])

const rules = {
  propertyType: [{ required: true, message: '请选择属性', trigger: 'change' }],
  paymentReason: [{ required: true, message: '请输入付款事由', trigger: 'change' }],
  orderId: [{ required: true, message: '请选择订单号', trigger: 'change' }],
  payForTypeName: [{ required: true, message: '费用类别必填', trigger: 'change' }],
  supplierName: [{ required: true, message: '供应商必填', trigger: 'change' }],
  paymentDate: [{ required: true, message: '请选择付款日期', trigger: 'change' }],
  paymentUnitId: [{ required: true, message: '请选择付款单位', trigger: 'change' }],
  applyUserId: [{ required: true, message: '请选择申请人', trigger: 'change' }],
  receiveUnit: [{ required: true, message: '请输入收款单位', trigger: 'blur' }],
  paymentAmount: [{ required: true, message: '请输入付款金额', trigger: 'blur' }]
}

const paymentRate = computed(() => {
  if (isModify.value) {
    return form.value.paymentAmount && choseOrderInfo.value.amount ? (form.value.paymentAmount / choseOrderInfo.value.amount) * 100 + '%' : ''
  } else {
    return props.paymentDetailInfo.paymentAmount && choseOrderInfo.value.amount ? (props.paymentDetailInfo.paymentAmount / choseOrderInfo.value.amount) * 100 + '%' : ''
  }
})

const upperYuan = computed(() => {
  if (isModify.value) {
    return form.value.paymentAmount ? digitUppercase(form.value.valuepaymentAmount) : ''
  } else {
    return props.paymentDetailInfo.paymentAmount ? digitUppercase(props.paymentDetailInfo.paymentAmount) : ''
  }
})

// function modifyInfo() {
//   isModify.value = true
//   resetForm()
// }

function closeDrawer() {
  isModify.value = false
  handleClose()
}

// function resetForm() {
//   if (formRef.value) {
//     formRef.value.resetFields()
//   }
//   const DataValue = JSON.parse(JSON.stringify(props.paymentDetailInfo))
//   DataValue.applyDate = String(DataValue.applyDate)
//   DataValue.paymentDate = String(DataValue.paymentDate)
//   DataValue.paymentUnitId = Number(DataValue.paymentUnitId)
//   DataValue.attachmentIds = DataValue.attachmentList ? DataValue.attachmentList.map((v) => v.id) : undefined
//   form.value = JSON.parse(JSON.stringify(DataValue))
//   useWatchFormValidate(formRef, form)
// }

watch(
  () => props.paymentDetailInfo.propertyType,
  (val) => {
    if (val) {
      getOrderInfoReset(val)
    } else {
      choseOrderInfo.value = {}
      orderListOption.value = []
    }
  },
  { deep: true, immediate: true }
)

async function getOrderInfoReset(type) {
  let data = {}
  try {
    data = await orderInfo(type)
  } catch (e) {
    console.log('获取订单信息', e)
  } finally {
    orderListOption.value = data
    choseOrderInfo.value = orderListOption.value.find(v => v.id === props.paymentDetailInfo.orderId)
  }
}

async function getOrderInfo(type) {
  let data = {}
  try {
    data = await orderInfo(type)
  } catch (e) {
    console.log('获取订单信息', e)
  } finally {
    orderListOption.value = data
    choseOrderInfo.value = {}
    clearInfo()
  }
}

function clearInfo() {
  form.value.orderSerialNumber = ''
  form.value.receiveBank = ''
  form.value.receiveBankAccount = ''
  form.value.receiveUnit = ''
  form.value.supplierId = ''
  form.value.supplierName = ''
  form.value.paymentBank = ''
  form.value.paymentBankAccount = ''
  form.value.paymentUnit = ''
  form.value.paymentUnitId = ''
  form.value.payForType = ''
  form.value.payForTypeName = ''
  form.value.basicClass = ''
  form.value.basicClassName = ''
}

function orderChange(val) {
  if (val) {
    choseOrderInfo.value = orderListOption.value.find(v => v.id === val)
    form.value.orderSerialNumber = choseOrderInfo.value.serialNumber
    form.value.receiveBank = choseOrderInfo.value.bankName
    form.value.receiveBankAccount = choseOrderInfo.value.bankAccount
    form.value.receiveUnit = choseOrderInfo.value.supplierName
    form.value.supplierId = choseOrderInfo.value.supplierId
    form.value.supplierName = choseOrderInfo.value.supplierName
    form.value.payForType = choseOrderInfo.value.payForType
    form.value.payForTypeName = choseOrderInfo.value.payForType ? contractPayForEnum.VL[choseOrderInfo.value.payForType] : ''
    form.value.basicClass = choseOrderInfo.value.basicClass
    form.value.basicClassName = choseOrderInfo.value.basicClassName
    form.value.paymentBank = ''
    form.value.paymentBankAccount = ''
    form.value.paymentUnit = ''
    form.value.paymentUnitId = ''
  } else {
    clearInfo()
  }
}

function orderCompanyChange(val) {
  if (val) {
    const orderVal = choseOrderInfo.value.companyBankAccountList.find(v => v.companyId === val)
    form.value.paymentBankAccount = orderVal.account
    form.value.paymentBank = orderVal.depositBank
    form.value.paymentUnit = orderVal.companyName
  } else {
    form.value.paymentBankAccount = ''
    form.value.paymentBank = ''
    form.value.paymentUnit = ''
  }
}

async function onSubmit(val) {
  try {
    if (props.type === 'detail') {
      const valid = await formRef.value.validate()
      if (valid) {
        form.value.attachmentIds = form.value.attachments ? form.value.attachments.map((v) => v.id) : undefined
        // 修改
      }
    } else {
      const submitData = {
        id: props.paymentDetailInfo.id,
        auditStatus: val
      }
      await editStatus(submitData)
      ElNotification({ title: '提交成功', type: 'success' })
      emit('success')
    }
  } catch (e) {
    console.log('付款信息', e)
  } finally {
    closeDrawer()
  }
}

</script>
<style lang="scss" scoped>
::v-deep(.el-input-number .el-input__inner) {
  text-align: left;
}
</style>
