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
      <common-button
        :loading="crud.status.cu === 2"
        type="primary"
        size="mini"
        @click="crud.submitCU"
      >确认</common-button>
    </template>
    <template #content>
      <el-form
        ref="formRef"
        :model="form"
        :rules="rules"
        size="small"
        label-width="140px"
      >
        <div style="display:flex;">
          <el-form-item
            label="属性"
            prop="propertyType"
          >
            <div style="width:260px;">
              <common-select
                v-model="form.propertyType"
                :options="supplierPayMentTypeEnum.ENUM"
                type="enum"
                size="small"
                clearable
                class="filter-item"
                placeholder="属性"
                style="width:250px"
              />
            </div>
          </el-form-item>
          <el-form-item
            label="付款事由"
            prop="paymentReason"
          >
            <div style="width:260px;">
              <common-select
                v-model="form.paymentReason"
                :options="dict.payment_reason"
                type="dict"
                clearable
                size="small"
                placeholder="付款事由"
                style="width:250px"
              />
            </div>
          </el-form-item>
        </div>
        <div style="display:flex;">
          <el-form-item
            :label="(form.propertyType===supplierPayMentTypeEnum.ENUM.PRODUCT.V || form.propertyType===supplierPayMentTypeEnum.ENUM.SUBCONTRACT.V)?'所属订单':'订单号'"
            prop="orderId"
          >
            <div style="width:260px;">
              <common-select
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
            </div>
          </el-form-item>
          <el-form-item
            label="付款方式"
            prop="payType"
          >
            <div style="width:260px;">
              <common-select
                v-model="form.payType"
                :options="supplierPayModeEnum.ENUM"
                type="enum"
                size="small"
                placeholder="付款方式"
                style="width: 250px;"
              />
            </div>
          </el-form-item>
        </div>
        <div style="display:flex;">
          <el-form-item
            v-if="form.propertyType!=supplierPayMentTypeEnum.ENUM.SUBCONTRACT.V"
            label="费用类别"
            prop="payForTypeName"
          >
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
          <el-form-item
            v-if="form.propertyType!=supplierPayMentTypeEnum.ENUM.SUBCONTRACT.V"
            label="种类"
            prop="basicClassName"
          >
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
        </div>
        <div style="display:flex;">
          <el-form-item
            label="供应商"
            prop="supplierName"
          >
            <div style="width:260px;">
              <el-input
                v-model="form.supplierName"
                type="text"
                placeholder="供应商"
                style="width: 250px;"
                disabled
              />
            </div>
          </el-form-item>
          <el-form-item
            label="付款日期"
            prop="paymentDate"
          >
            <div style="width:260px;">
              <el-date-picker
                v-model="form.paymentDate"
                type="date"
                value-format="x"
                placeholder="选择付款日期"
                style="width: 250px;"
              />
            </div>
          </el-form-item>
        </div>
        <div style="display:flex;">
          <el-form-item
            v-if="form.propertyType !=supplierPayMentTypeEnum.ENUM.MATERIAL_TRANSPORT.V && form.propertyType !=supplierPayMentTypeEnum.ENUM.PRODUCT_TRANSPORT.V"
            label="合同金额(元)"
            prop="contractAmount"
          >
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
          <el-form-item
            v-if="form.propertyType===supplierPayMentTypeEnum.ENUM.MATERIAL_TRANSPORT.V || form.propertyType===supplierPayMentTypeEnum.ENUM.PRODUCT_TRANSPORT.V "
            label="运费金额(元)"
            prop="contractAmount"
          >
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
          <el-form-item
            label="付款单位"
            prop="paymentUnitId"
          >
            <div style="width:260px;">
              <common-select
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
            </div>
          </el-form-item>
        </div>
        <div style="display:flex;">
          <el-form-item label="入库累计(元)">
            <div style="width:260px;">
              <el-input
                v-model="choseOrderInfo.inBoundAmount"
                type="text"
                placeholder="入库累计(元)"
                style="width: 250px;"
                disabled
              />
            </div>
          </el-form-item>
        </div>
        <div style="display:flex;">
          <el-form-item
            label="申请人"
            prop="applyUserId"
          >
            <div style="width:260px;">
              <user-dept-cascader
                v-model="form.applyUserId"
                filterable
                clearable
                show-all-levels
                style="width:250px"
                placeholder="申请人"
              />
            </div>
          </el-form-item>
          <el-form-item
            label="付款行"
            prop="paymentBank"
          >
            <div style="width:260px;">
              <el-input
                v-model="form.paymentBank"
                type="text"
                placeholder="付款行"
                style="width: 250px;"
              />
            </div>
          </el-form-item>
        </div>
        <div style="display:flex;">
          <el-form-item
            label="申请日期"
            prop="applyDate"
          >
            <div style="width:260px;">
              <el-date-picker
                v-model="form.applyDate"
                type="date"
                value-format="x"
                placeholder="选择申请日期"
                style="width: 250px;"
              />
            </div>
          </el-form-item>
          <el-form-item
            label="付款账号"
            prop="paymentBankAccount"
          >
            <div style="width:260px;">
              <el-input
                v-model="form.paymentBankAccount"
                type="text"
                placeholder="付款账号"
                style="width: 250px;"
              />
            </div>
          </el-form-item>
        </div>
        <div style="display:flex;">
          <el-form-item
            label="申请金额(元)"
            prop="applyAmount"
          >
            <div style="width:260px;">
              <el-input-number
                v-model.number="form.applyAmount"
                :min="0"
                :max="choseOrderInfo.amount? choseOrderInfo.amount : 99999999999"
                :step="10000"
                :precision="DP.YUAN"
                placeholder="申请金额(元)"
                controls-position="right"
                style="width: 250px;"
              />
            </div>
          </el-form-item>
          <el-form-item
            label="收款单位"
            prop="receiveUnit"
          >
            <div style="width:260px;">
              <el-input
                v-model="form.receiveUnit"
                type="text"
                placeholder="收款方"
                style="width: 250px;"
              />
            </div>
          </el-form-item>
        </div>
        <div style="display:flex;">
          <el-form-item
            label="付款金额(元)"
            prop="paymentAmount"
          >
            <div style="width:260px;">
              <el-input-number
                v-model.number="form.paymentAmount"
                :min="0"
                :max="choseOrderInfo.amount? choseOrderInfo.amount : 99999999999"
                :step="10000"
                :precision="DP.YUAN"
                placeholder="付款金额(元)"
                controls-position="right"
                style="width: 250px;"
              />
            </div>
            <div style="width:260px;">
              <el-input
                v-model="upperYuan"
                type="text"
                placeholder="付款金额大写"
                style="width: 250px;"
                disabled
              />
            </div>
          </el-form-item>
          <el-form-item
            label="收款行"
            prop="paymentBank"
          >
            <div style="width:260px;">
              <el-input
                v-model="form.paymentBank"
                type="text"
                placeholder="收款方"
                style="width: 250px;"
              />
            </div>
          </el-form-item>
        </div>
        <div style="display:flex;">
          <el-form-item
            label="付款比例"
            prop="paymentRate"
          >
            <div style="width:260px;">
              <el-input
                v-model="paymentRate"
                type="text"
                placeholder="付款比例"
                style="width: 250px;"
                disabled
              />
            </div>
          </el-form-item>
          <el-form-item
            label="收款账号"
            prop="receiveBankAccount"
          >
            <div style="width:260px;">
              <el-input
                v-model="form.receiveBankAccount"
                type="text"
                placeholder="收款账号"
                style="width: 250px;"
              />
            </div>
          </el-form-item>
        </div>
        <div style="display:flex;">
          <el-form-item
            v-if="form.propertyType===supplierPayMentTypeEnum.ENUM.PRODUCT.V || form.propertyType===supplierPayMentTypeEnum.ENUM.SUBCONTRACT.V"
            label="关联项目"
            prop="projectId"
          >
            <div style="width:260px;">
              <template v-if="choseOrderInfo.projectList && choseOrderInfo.projectList.length>0">
                <div
                  v-for="item in choseOrderInfo.projectList"
                  :key="item.id"
                >{{item.serialNumber+' '+item.shortName}}</div>
              </template>
            </div>
          </el-form-item>
          <el-form-item label="附件">
            <div style="width:260px;">
              <upload-btn
                ref="uploadRef"
                v-model:files="form.attachments"
                :file-classify="fileClassifyEnum.ENUM.NORMAL.V"
                :accept="'.pdf,.jpg,.jpeg,.png'"
                :tip="'支持扩展名:pdf .jpg .jpeg .png'"
              />
            </div>
          </el-form-item>
        </div>
        <el-form-item
          label="备注"
          prop="remark"
        >
          <el-input
            v-model="form.remark"
            type="textarea"
            :autosize="{ minRows: 6, maxRows: 8}"
            placeholder="可填写备注"
            style="max-width: 500px;"
          />
        </el-form-item>
      </el-form>
    </template>
  </common-drawer>
</template>

<script setup>
import { ref, watch, computed } from 'vue'
import { regForm } from '@compos/use-crud'
import useDict from '@compos/store/use-dict'
import { DP } from '@/settings/config'
import { supplierPayMentTypeEnum, contractPayForEnum, supplierPayModeEnum } from '@enum-ms/contract'
import { orderInfo } from '@/api/contract/supplier-manage/pay-invoice/pay'
import { digitUppercase } from '@/utils/data-type/number'
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

const { CRUD, crud, form } = regForm(defaultForm, formRef)

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
const upperYuan = computed(() => {
  return form.paymentAmount ? digitUppercase(form.paymentAmount) : ''
})
const paymentRate = computed(() => {
  return form.paymentAmount && choseOrderInfo.value.amount ? (form.paymentAmount / choseOrderInfo.value.amount) * 100 + '%' : ''
})
watch(
  () => form.propertyType,
  (val) => {
    if (val) {
      getOrderInfo(val)
    } else {
      choseOrderInfo.value = {}
      orderListOption.value = []
    }
  },
  { deep: true, immediate: true }
)

async function getOrderInfo(type) {
  let data = {}
  try {
    data = await orderInfo(type)
    // data = [
    //   {
    //     "amount": 1000, //合同额
    //     "bankAccount": "1", //收款银行账户
    //     "bankName": "1",
    //     "basicClass": 1, //物料种类
    //     "basicClassName": "钢", //物料种类名称
    //     "companyBankAccountList": [
    //       {companyId: 1, companyName: "河南六建重工有限公司", depositBank: "招商银行", account: "888288828182818281212"},
    //       {companyId: 2, companyName: "河南六建建筑集团有限公司", depositBank: "徽商银行", account: "213121423534514351451"},
    //       {companyId: 3, companyName: "河南六建集团钢结构分公司", depositBank: "321321", account: "3213213"}
    //     ],
    //     "projectList": [
    //       {
    //         "id": 1,
    //         "name": "a1",
    //         "serialNumber": "aaa",
    //         "shortName": "a11"
    //       },
    //       {
    //         "id": 2,
    //         "name": "a2",
    //         "serialNumber": "aaa2",
    //         "shortName": "a22"
    //       }
    //     ],
    //     "id": 1, //订单id
    //     "inBoundAmount": 500, //累计收款
    //     "orderProperty": 1, //订单属性
    //     "payForType": 1,  //费用用途 1货款 2运费
    //     "serialNumber": "a1", //订单号
    //     "supplierId": 1,
    //     "supplierName": "供应商1" //供应商名称
    //   },
    //   {
    //     "amount": 2500, //合同额
    //     "bankAccount": "2", //收款银行账户
    //     "bankName": "2",
    //     "basicClass": 1, //物料种类
    //     "basicClassName": "钢", //物料种类名称
    //     "companyBankAccountList": [
    //       {companyId: 1, companyName: "河南六建重工有限公司", depositBank: "招商银行", account: "888288828182818281212"},
    //       {companyId: 2, companyName: "河南六建建筑集团有限公司", depositBank: "徽商银行", account: "213121423534514351451"},
    //       {companyId: 3, companyName: "河南六建集团钢结构分公司", depositBank: "321321", account: "3213213"}
    //     ],
    //     "projectList": [
    //       {
    //         "id": 1,
    //         "name": "a1",
    //         "serialNumber": "aaa",
    //         "shortName": "a11"
    //       },
    //       {
    //         "id": 2,
    //         "name": "a2",
    //         "serialNumber": "aaa2",
    //         "shortName": "a22"
    //       }
    //     ],
    //     "id": 2, //订单id
    //     "inBoundAmount": 500, //累计收款
    //     "orderProperty": 1, //订单属性
    //     "payForType": 1,  //费用用途 1货款 2运费
    //     "serialNumber": "a2", //订单号
    //     "supplierId": 2,
    //     "supplierName": "供应商2" //供应商名称
    //   }
    // ]
  } catch (e) {
    console.log('获取订单信息', e)
  } finally {
    orderListOption.value = data
    choseOrderInfo.value = {}
    clearInfo()
  }
}

function clearInfo() {
  form.orderId = ''
  form.orderSerialNumber = ''
  form.receiveBank = ''
  form.receiveBankAccount = ''
  form.receiveUnit = ''
  form.supplierId = ''
  form.supplierName = ''
  form.paymentBank = ''
  form.paymentBankAccount = ''
  form.paymentUnit = ''
  form.paymentUnitId = ''
  form.payForType = ''
  form.payForTypeName = ''
  form.basicClass = ''
  form.basicClassName = ''
}

function orderChange(val) {
  if (val) {
    choseOrderInfo.value = orderListOption.value.find((v) => v.id === val)
    form.orderSerialNumber = choseOrderInfo.value.serialNumber
    form.receiveBank = choseOrderInfo.value.bankName
    form.receiveBankAccount = choseOrderInfo.value.bankAccount
    form.receiveUnit = choseOrderInfo.value.supplierName
    form.supplierId = choseOrderInfo.value.supplierId
    form.supplierName = choseOrderInfo.value.supplierName
    form.payForType = choseOrderInfo.value.payForType
    form.payForTypeName = choseOrderInfo.value.payForType ? contractPayForEnum.VL[choseOrderInfo.value.payForType] : ''
    form.basicClass = choseOrderInfo.value.basicClass
    form.basicClassName = choseOrderInfo.value.basicClassName
    form.paymentBank = ''
    form.paymentBankAccount = ''
    form.paymentUnit = ''
    form.paymentUnitId = ''
  } else {
    clearInfo()
  }
}

function orderCompanyChange(val) {
  if (val) {
    const orderVal = choseOrderInfo.value.companyBankAccountList.find((v) => v.companyId === val)
    form.paymentBankAccount = orderVal.account
    form.paymentBank = orderVal.depositBank
    form.paymentUnit = orderVal.companyName
  } else {
    form.paymentBankAccount = ''
    form.paymentBank = ''
    form.paymentUnit = ''
  }
}

CRUD.HOOK.beforeSubmit = (crud, form) => {
  crud.form.attachmentIds = crud.form.attachments ? crud.form.attachments.map((v) => v.id) : undefined
}
</script>
<style lang="scss" scoped>
::v-deep(.el-input-number .el-input__inner) {
  text-align: left;
}
</style>
