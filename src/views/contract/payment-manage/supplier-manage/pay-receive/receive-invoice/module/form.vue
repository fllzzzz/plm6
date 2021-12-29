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
        <div
          class="form-row"
          style="display:flex;"
        >
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
                @change="getOrderInfo"
              />
            </div>
          </el-form-item>
          <el-form-item
            label="发票类型"
            prop="invoiceType"
          >
            <common-select
              v-model="form.invoiceType"
              :options="invoiceTypeEnum.ENUM"
              type="enum"
              size="small"
              clearable
              class="filter-item"
              placeholder="发票类型"
              style="width:250px"
            />
          </el-form-item>
        </div>
        <div
          class="form-row"
          style="display:flex;"
        >
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
            label="进项税额"
            prop="taxRate"
            v-if="form.invoiceType===invoiceTypeEnum.ENUM.SPECIAL.V"
          >
            <div style="width:260px;">
              <el-input
                v-model="rateMoney"
                type="text"
                placeholder="先输入税率"
                style="width: 160px;"
                disabled
              />
              <el-input-number
                v-model="form.taxRate"
                :step="1"
                :min="0"
                :max="100"
                :precision="DP.ACCOUNTING"
                :controls="false"
                controls-position="right"
                class="input-underline"
                style="width:70px"
                placeholder="0-100"
              />%
            </div>
          </el-form-item>
        </div>
        <div
          class="form-row"
          style="display:flex;"
        >
          <el-form-item
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
          <el-form-item
            label="发票日期"
            prop="invoiceDate"
          >
            <el-date-picker
              v-model="form.invoiceDate"
              type="date"
              value-format="x"
              placeholder="选择发票日期"
              style="width: 250px;"
            />
          </el-form-item>
        </div>
        <div
          class="form-row"
          style="display:flex;"
        >
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
            label="发票号码"
            prop="invoiceSerialNumber"
          >
            <el-input
              v-model="form.invoiceSerialNumber"
              type="text"
              placeholder="发票号码"
              style="width: 250px;"
            />
          </el-form-item>
        </div>
        <div
          class="form-row"
          style="display:flex;"
        >
          <el-form-item
            label="合同金额(元)"
            prop="amount"
          >
            <el-input
              v-model="choseOrderInfo.amount"
              type="text"
              placeholder="合同金额"
              style="width: 250px;"
              disabled
            />
          </el-form-item>
          <el-form-item
            label="发票面额(元)"
            prop="invoiceAmount"
          >
            <el-input-number
              v-model.number="form.invoiceAmount"
              :min="-99999999999"
              :max="99999999999"
              :step="10000"
              :precision="DP.YUAN"
              placeholder="本次收款金额(元)"
              controls-position="right"
              style="width: 250px;"
            />
          </el-form-item>
        </div>
        <div
          class="form-row"
          style="display:flex;"
        >
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
        </div>
        <div
          class="form-row"
          style="display:flex;"
        >
          <el-form-item
            label="收票日期"
            prop="receiveInvoiceDate"
          >
            <el-date-picker
              v-model="form.receiveInvoiceDate"
              type="date"
              value-format="x"
              placeholder="选择收票日期"
              style="width: 250px;"
            />
          </el-form-item>
          <el-form-item
            label="收票单位"
            prop="receiveInvoiceUnitId"
          >
            <common-select
              v-model="form.receiveInvoiceUnitId"
              :options="choseOrderInfo.companyBankAccountList"
              :type="'other'"
              :dataStructure="typeProp"
              size="small"
              clearable
              class="filter-item"
              placeholder="收票单位"
              style="width:250px"
              @change="orderCompanyChange"
            />
          </el-form-item>
        </div>
        <div
          class="form-row"
          style="display:flex;"
        >
          <el-form-item
            label="附件"
            prop="attachments"
          >
            <div style="width:260px">
              <upload-btn
                ref="uploadRef"
                v-model:files="form.attachments"
                :file-classify="fileClassifyEnum.CONTRACT_ATT.V"
                :limit="1"
                :accept="'.pdf,.jpg,.jpeg,.png'"
                :tip="'支持扩展名:pdf .jpg .jpeg .png'"
              />
            </div>
          </el-form-item>
          <el-form-item
            label="开票单位"
            prop="invoiceUnit"
          >
            <el-input
              v-model="form.invoiceUnit"
              type="text"
              placeholder="开票单位"
              style="width: 250px;"
            />
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
            :maxLength="500"
            placeholder="可填写备注"
            style="max-width: 500px;"
          />
        </el-form-item>
      </el-form>
    </template>
  </common-drawer>
</template>

<script setup>
import { ref, computed, watch } from 'vue'
import { regForm } from '@compos/use-crud'
import { DP } from '@/settings/config'
import { invoiceTypeEnum } from '@enum-ms/finance'
import { fileClassifyEnum } from '@enum-ms/file'
import { supplierPayMentTypeEnum } from '@enum-ms/contract'
import { orderInfo } from '@/api/contract/supplier-manage/pay-invoice/pay'
import UploadBtn from '@comp/file-upload/UploadBtn'

const formRef = ref()
const uploadRef = ref()
const defaultForm = {
  id: undefined,
  attachmentIds: undefined,
  attachments: undefined,
  basicClass: undefined,
  basicClassName: undefined,
  invoiceAmount: undefined,
  invoiceDate: undefined,
  invoiceSerialNumber: undefined,
  invoiceType: undefined,
  invoiceUnit: undefined,
  orderId: undefined,
  orderSerialNumber: undefined,
  propertyType: undefined,
  receiveInvoiceDate: undefined,
  receiveInvoiceUnit: undefined,
  receiveInvoiceUnitId: undefined,
  remark: undefined,
  supplierId: undefined,
  supplierName: undefined,
  tax: undefined,
  taxRate: undefined
}

const { CRUD, crud, form } = regForm(defaultForm, formRef)
const orderProp = { key: 'id', label: 'serialNumber', value: 'id' }
const typeProp = { key: 'companyId', label: 'companyName', value: 'companyId' }
const choseOrderInfo = ref({})
const orderListOption = ref([])

const rules = {
  propertyType: [{ required: true, message: '请选择属性', trigger: 'change' }],
  invoiceType: [{ required: true, message: '请选择发票类型', trigger: 'change' }],
  orderId: [{ required: true, message: '请选择订单号', trigger: 'change' }],
  taxRate: [{ required: true, message: '请输入税率', trigger: 'change', type: 'number' }],
  invoiceDate: [{ required: true, message: '请选择开票日期', trigger: 'change' }],
  supplierName: [{ required: true, message: '供应商必填', trigger: 'change' }],
  invoiceAmount: [{ required: true, message: '请输入发票面额', trigger: 'change', type: 'number' }],
  receiveInvoiceDate: [{ required: true, message: '请选择收票日期', trigger: 'change' }],
  receiveInvoiceUnitId: [{ required: true, message: '请选择收票单位', trigger: 'change' }]
}

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
  form.basicClass = undefined
  form.basicClassName = undefined
  form.orderId = undefined
  form.orderSerialNumber = undefined
  form.receiveInvoiceUnit = undefined
  form.receiveInvoiceUnitId = undefined
  form.supplierId = undefined
  form.supplierName = undefined
  form.invoiceUnit = undefined
}

function orderChange(val) {
  if (val) {
    choseOrderInfo.value = orderListOption.value.find((v) => v.id === val)
    form.orderSerialNumber = choseOrderInfo.value.serialNumber
    form.basicClass = choseOrderInfo.value.basicClass
    form.basicClassName = choseOrderInfo.value.basicClassName
    form.supplierId = choseOrderInfo.value.supplierId
    form.supplierName = choseOrderInfo.value.supplierName
    form.invoiceUnit = choseOrderInfo.value.supplierName
    form.receiveInvoiceUnit = undefined
    form.receiveInvoiceUnitId = undefined
  } else {
    clearInfo()
  }
}

function orderCompanyChange(val) {
  if (val) {
    const orderVal = choseOrderInfo.value.companyBankAccountList.find((v) => v.companyId === val)
    form.receiveInvoiceUnit = orderVal.companyName
  } else {
    form.receiveInvoiceUnit = undefined
  }
}

const rateMoney = computed(() => {
  return choseOrderInfo.value.amount && form.taxRate ? ((choseOrderInfo.value.amount * form.taxRate) / 100).toFixed(DP.YUAN) : ''
})

CRUD.HOOK.beforeSubmit = (crud, form) => {
  crud.form.tax = rateMoney.value || ''
  crud.form.attachmentIds = crud.form.attachments ? crud.form.attachments.map((v) => v.id) : undefined
}
</script>
<style lang="scss" scoped>
::v-deep(.el-input-number .el-input__inner) {
  text-align: left;
}
</style>
