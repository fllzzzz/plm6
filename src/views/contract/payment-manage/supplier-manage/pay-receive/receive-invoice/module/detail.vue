<template>
  <common-drawer
    append-to-body
    v-model="visible"
    top="10vh"
    width="600px"
    :before-close="closeDrawer"
    title="开票信息"
    :wrapper-closable="false"
    size="860px"
  >
    <template #title>
      <div class="dialog-title">
        <span style="margin-right: 5px">开票信息</span>
         <common-button v-if="collectionInfo.auditStatus" size="mini" :type="collectionInfo.auditStatus==auditTypeEnum.ENUM.REJECT.V?'info':(collectionInfo.auditStatus==auditTypeEnum.ENUM.PASS.V?'success':'warning')">
          {{ collectionInfo.auditStatus==auditTypeEnum.ENUM.REJECT.V?'已驳回':(collectionInfo.auditStatus==auditTypeEnum.ENUM.PASS.V?'已通过':'审核中') }}
        </common-button>
        <span style="position:absolute;right:20px;">
          <template v-if="collectionInfo.auditStatus">
            <template v-if="!isModify">
              <common-button v-if="collectionInfo.auditStatus==auditTypeEnum.ENUM.AUDITING.V && type==='audit'" size="small" type="info" @click="onSubmit(auditTypeEnum.ENUM.REJECT.V)">驳回</common-button>
              <common-button v-if="collectionInfo.auditStatus==auditTypeEnum.ENUM.AUDITING.V && type==='audit'" size="small" type="success" @click="onSubmit(auditTypeEnum.ENUM.PASS.V)">通过</common-button>
              <!-- <common-button size="small" type="primary" @click="modifyInfo" v-if="collectionInfo.auditStatus==auditTypeEnum.ENUM.REJECT.V && type==='detail'">重新编辑</common-button> -->
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
        <div class="form-row" style="display: flex">
          <el-form-item label="属性" prop="propertyType">
            <div style="width: 260px">
              <common-select
                v-if="isModify"
                v-model="form.propertyType"
                :options="supplierPayMentTypeEnum.ENUM"
                type="enum"
                size="small"
                clearable
                class="filter-item"
                placeholder="属性"
                style="width: 250px"
                @change="getOrderInfo"
              />
              <span v-else>{{ collectionInfo.propertyType ? supplierPayMentTypeEnum.VL[collectionInfo.propertyType] : '' }}</span>
            </div>
          </el-form-item>
          <el-form-item label="发票类型" prop="invoiceType">
            <common-select
              v-if="isModify"
              v-model="form.invoiceType"
              :options="invoiceTypeEnum.ENUM"
              type="enum"
              size="small"
              clearable
              class="filter-item"
              placeholder="发票类型"
              style="width: 250px"
            />
            <template v-else>
              <span>{{ collectionInfo.invoiceType ? invoiceTypeEnum.VL[collectionInfo.invoiceType] : '' }}</span>
              <span v-if="collectionInfo.invoiceType === invoiceTypeEnum.ENUM.SPECIAL.V">{{ `(${collectionInfo.taxRate}%)` }}</span>
            </template>
          </el-form-item>
        </div>
        <div class="form-row" style="display: flex">
          <template v-if="isModify">
            <el-form-item :label="(form.propertyType===supplierPayMentTypeEnum.ENUM.PRODUCT.V || form.propertyType===supplierPayMentTypeEnum.ENUM.SUBCONTRACT.V)?'所属订单':'订单号'" prop="orderId">
              <div style="width: 260px">
                <common-select
                  v-model="form.orderId"
                  :options="orderListOption"
                  :type="'other'"
                  :dataStructure="orderProp"
                  size="small"
                  clearable
                  class="filter-item"
                  placeholder="订单号"
                  style="width: 250px"
                  @change="orderChange"
                />
              </div>
            </el-form-item>
          </template>
          <template v-else>
            <el-form-item :label="(collectionInfo.propertyType===supplierPayMentTypeEnum.ENUM.PRODUCT.V || collectionInfo.propertyType===supplierPayMentTypeEnum.ENUM.SUBCONTRACT.V)?'所属订单':'订单号'" prop="orderId">
              <div style="width: 260px">
                <span>{{ collectionInfo.orderSerialNumber }}</span>
              </div>
            </el-form-item>
          </template>
          <template v-if="isModify">
            <el-form-item label="进项税额" prop="taxRate" v-if="form.invoiceType === invoiceTypeEnum.ENUM.SPECIAL.V">
              <div style="width: 260px">
                <el-input v-model="rateMoney" type="text" placeholder="先输入税率" style="width: 160px" disabled />
                <el-input-number
                  v-model="form.taxRate"
                  :step="1"
                  :min="0"
                  :max="100"
                  :precision="DP.ACCOUNTING"
                  :controls="false"
                  controls-position="right"
                  class="input-underline"
                  style="width: 70px"
                  placeholder="0-100"
                />%
              </div>
            </el-form-item>
          </template>
          <template v-else>
            <el-form-item label="销项税额" prop="taxRate" v-if="collectionInfo.invoiceType === invoiceTypeEnum.ENUM.SPECIAL.V">
              <div style="width: 260px">
                <span>{{ rateMoney ? toThousand(rateMoney) : '' }}</span>
              </div>
            </el-form-item>
          </template>
        </div>
        <div class="form-row" style="display: flex">
          <el-form-item label="种类" prop="basicClassName">
            <div style="width: 260px">
              <el-input v-if="isModify" v-model="form.basicClassName" type="text" placeholder="种类" style="width: 250px" disabled />
              <span v-else>{{ collectionInfo.basicClassName }}</span>
            </div>
          </el-form-item>
          <el-form-item label="发票日期" prop="invoiceDate">
            <div style="width: 260px">
              <el-date-picker
                v-if="isModify"
                v-model="form.invoiceDate"
                type="date"
                value-format="x"
                placeholder="选择发票日期"
                style="width: 250px"
              />
              <template v-else>
                <span>{{ collectionInfo.invoiceDate?parseTime(collectionInfo.invoiceDate,'{y}-{m}-{d}'):'-' }}</span>
              </template>
            </div>
          </el-form-item>
        </div>
        <div class="form-row" style="display: flex">
          <el-form-item label="供应商" prop="supplierName">
            <div style="width: 260px">
              <el-input v-if="isModify" v-model="form.supplierName" type="text" placeholder="供应商" style="width: 250px" disabled />
              <span v-else>{{ collectionInfo.supplierName }}</span>
            </div>
          </el-form-item>
          <el-form-item label="发票号码" prop="invoiceSerialNumber">
            <div style="width: 260px">
              <el-input v-if="isModify" v-model="form.invoiceSerialNumber" type="text" placeholder="发票号码" style="width: 250px" />
              <span v-else>{{ collectionInfo.invoiceSerialNumber }}</span>
            </div>
          </el-form-item>
        </div>
        <div class="form-row" style="display: flex">
          <el-form-item label="合同金额(元)" prop="amount">
            <div style="width: 260px">
              <el-input v-if="isModify" v-model="choseOrderInfo.amount" type="text" placeholder="合同金额" style="width: 250px" disabled />
              <span v-else>{{ choseOrderInfo.amount ? toThousand(choseOrderInfo.amount) : '' }}</span>
            </div>
          </el-form-item>
          <el-form-item label="发票面额(元)" prop="invoiceAmount">
            <div style="width: 260px">
              <el-input-number
                v-if="isModify"
                v-model.number="form.invoiceAmount"
                :min="-99999999999"
                :max="99999999999"
                :step="10000"
                :precision="DP.YUAN"
                placeholder="本次收款金额(元)"
                controls-position="right"
                style="width: 250px"
              />
              <span v-else>{{ collectionInfo.invoiceAmount ? toThousand(collectionInfo.invoiceAmount) : '' }}</span>
            </div>
          </el-form-item>
        </div>
        <div class="form-row" style="display: flex">
          <template v-if="isModify">
            <el-form-item
              v-if="
                form.propertyType === supplierPayMentTypeEnum.ENUM.PRODUCT.V ||
                form.propertyType === supplierPayMentTypeEnum.ENUM.SUBCONTRACT.V
              "
              label="关联项目"
              prop="projectId"
            >
              <div style="width: 260px">
                <template v-if="choseOrderInfo.projectList && choseOrderInfo.projectList.length > 0">
                  <div v-for="item in choseOrderInfo.projectList" :key="item.id">{{ item.serialNumber + ' ' + item.shortName }}</div>
                </template>
              </div>
            </el-form-item>
          </template>
          <template v-else>
            <el-form-item
              v-if="
                collectionInfo.propertyType === supplierPayMentTypeEnum.ENUM.PRODUCT.V ||
                collectionInfo.propertyType === supplierPayMentTypeEnum.ENUM.SUBCONTRACT.V
              "
              label="关联项目"
              prop="projectId"
            >
              <div style="width: 260px">
                <template v-if="choseOrderInfo.projectList && choseOrderInfo.projectList.length > 0">
                  <div v-for="item in choseOrderInfo.projectList" :key="item.id">{{ item.serialNumber + ' ' + item.shortName }}</div>
                </template>
              </div>
            </el-form-item>
          </template>
        </div>
        <div class="form-row" style="display: flex">
          <el-form-item label="收票日期" prop="receiveInvoiceDate">
            <div style="width: 260px">
              <el-date-picker
                v-if="isModify"
                v-model="form.receiveInvoiceDate"
                type="date"
                value-format="x"
                placeholder="选择收票日期"
                style="width: 250px"
              />
              <template v-else>
                <span>{{ collectionInfo.receiveInvoiceDate? parseTime(collectionInfo.receiveInvoiceDate,'{y}-{m}-{d}'): '-' }}</span>
              </template>
            </div>
          </el-form-item>
          <el-form-item label="收票单位" prop="receiveInvoiceUnitId">
            <div style="width: 260px">
              <common-select
                v-if="isModify"
                v-model="form.receiveInvoiceUnitId"
                :options="choseOrderInfo.companyBankAccountList"
                :type="'other'"
                :dataStructure="typeProp"
                size="small"
                clearable
                class="filter-item"
                placeholder="收票单位"
                style="width: 250px"
                @change="orderCompanyChange"
              />
              <span v-else>{{ collectionInfo.receiveInvoiceUnit }}</span>
            </div>
          </el-form-item>
        </div>
        <div class="form-row" style="display: flex">
          <el-form-item label="附件" prop="attachments">
            <div style="width: 260px">
              <upload-btn
                ref="uploadRef"
                v-model:files="form.attachments"
                :file-classify="fileClassifyEnum.CONTRACT_ATT.V"
                :limit="1"
                :accept="'.pdf,.jpg,.jpeg,.png'"
                :tip="'支持扩展名:pdf .jpg .jpeg .png'"
                v-if="isModify"
              />
              <template v-else>
                <div v-for="item in collectionInfo.attachmentList" :key="item.id">{{ item.name }}</div>
              </template>
            </div>
          </el-form-item>
          <el-form-item label="开票单位" prop="invoiceUnit">
            <div style="width: 260px">
              <el-input v-model="form.invoiceUnit" type="text" placeholder="开票单位" style="width: 250px" v-if="isModify" />
              <span v-else>{{ collectionInfo.invoiceUnit }}</span>
            </div>
          </el-form-item>
        </div>
        <el-form-item label="备注" prop="remark">
          <el-input
            v-if="isModify"
            v-model="form.remark"
            type="textarea"
            :autosize="{ minRows: 6, maxRows: 8 }"
            :maxlength="500"
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
import { DP } from '@/settings/config'
import { orderInfo } from '@/api/contract/supplier-manage/pay-invoice/pay'
// import useWatchFormValidate from '@compos/form/use-watch-form-validate'
import useVisible from '@compos/use-visible'
import { invoiceTypeEnum } from '@enum-ms/finance'
import { fileClassifyEnum } from '@enum-ms/file'
import { supplierPayMentTypeEnum, auditTypeEnum } from '@enum-ms/contract'
import { editStatus } from '@/api/contract/supplier-manage/pay-invoice/invoice'
import { ElNotification } from 'element-plus'
import UploadBtn from '@/components/file-upload/UploadBtn'
import { toThousand } from '@data-type/number'
import { parseTime } from '@/utils/date'

const formRef = ref()
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
const orderProp = { key: 'id', label: 'serialNumber', value: 'id' }
const typeProp = { key: 'companyId', label: 'companyName', value: 'companyId' }
const choseOrderInfo = ref({})
const orderListOption = ref([])
const isModify = ref(false)
const emit = defineEmits(['success', 'update:modelValue'])
const { visible, handleClose } = useVisible({ emit, props })

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
  () => props.collectionInfo.propertyType,
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

// function resetForm() {
//   if (formRef.value) {
//     formRef.value.resetFields()
//   }
//   const DataValue = JSON.parse(JSON.stringify(props.collectionInfo))
//   DataValue.invoiceDate = String(DataValue.invoiceDate)
//   DataValue.receiveInvoiceDate = String(DataValue.receiveInvoiceDate)
//   DataValue.receiveInvoiceUnitId = Number(DataValue.receiveInvoiceUnitId)
//   DataValue.attachmentIds = DataValue.attachmentList ? DataValue.attachmentList.map((v) => v.id) : undefined
//   form.value = JSON.parse(JSON.stringify(DataValue))
//   useWatchFormValidate(formRef, form)
// }

// function modifyInfo() {
//   isModify.value = true
//   resetForm()
// }

function closeDrawer() {
  isModify.value = false
  handleClose()
}

async function getOrderInfoReset(type) {
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
    choseOrderInfo.value = orderListOption.value.find(v => v.id === props.collectionInfo.orderId)
  }
}

async function getOrderInfo(type) {
  let data = {}
  try {
    data = await orderInfo(type)
    // data = [
    //   {
    //     amount: 1000, //合同额
    //     bankAccount: '1', //收款银行账户
    //     bankName: '1',
    //     basicClass: 1, //物料种类
    //     basicClassName: '钢', //物料种类名称
    //     companyBankAccountList: [
    //       { companyId: 1, companyName: '河南六建重工有限公司', depositBank: '招商银行', account: '888288828182818281212' },
    //       { companyId: 2, companyName: '河南六建建筑集团有限公司', depositBank: '徽商银行', account: '213121423534514351451' },
    //       { companyId: 3, companyName: '河南六建集团钢结构分公司', depositBank: '321321', account: '3213213' },
    //     ],
    //     projectList: [
    //       {
    //         id: 1,
    //         name: 'a1',
    //         serialNumber: 'aaa',
    //         shortName: 'a11',
    //       },
    //       {
    //         id: 2,
    //         name: 'a2',
    //         serialNumber: 'aaa2',
    //         shortName: 'a22',
    //       },
    //     ],
    //     id: 1, //订单id
    //     inBoundAmount: 500, //累计收款
    //     orderProperty: 1, //订单属性
    //     payForType: 1, //费用用途 1货款 2运费
    //     serialNumber: 'a1', //订单号
    //     supplierId: 1,
    //     supplierName: '供应商1', //供应商名称
    //   },
    //   {
    //     amount: 2500, //合同额
    //     bankAccount: '2', //收款银行账户
    //     bankName: '2',
    //     basicClass: 1, //物料种类
    //     basicClassName: '钢', //物料种类名称
    //     companyBankAccountList: [
    //       { companyId: 1, companyName: '河南六建重工有限公司', depositBank: '招商银行', account: '888288828182818281212' },
    //       { companyId: 2, companyName: '河南六建建筑集团有限公司', depositBank: '徽商银行', account: '213121423534514351451' },
    //       { companyId: 3, companyName: '河南六建集团钢结构分公司', depositBank: '321321', account: '3213213' },
    //     ],
    //     projectList: [
    //       {
    //         id: 1,
    //         name: 'a1',
    //         serialNumber: 'aaa',
    //         shortName: 'a11',
    //       },
    //       {
    //         id: 2,
    //         name: 'a2',
    //         serialNumber: 'aaa2',
    //         shortName: 'a22',
    //       },
    //     ],
    //     id: 2, //订单id
    //     inBoundAmount: 500, //累计收款
    //     orderProperty: 1, //订单属性
    //     payForType: 1, //费用用途 1货款 2运费
    //     serialNumber: 'a2', //订单号
    //     supplierId: 2,
    //     supplierName: '供应商2', //供应商名称
    //   },
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
  form.value.basicClass = undefined
  form.value.basicClassName = undefined
  form.value.orderId = undefined
  form.value.orderSerialNumber = undefined
  form.value.receiveInvoiceUnit = undefined
  form.value.receiveInvoiceUnitId = undefined
  form.value.supplierId = undefined
  form.value.supplierName = undefined
  form.value.invoiceUnit = undefined
}

function orderChange(val) {
  if (val) {
    choseOrderInfo.value = orderListOption.value.find((v) => v.id === val)
    form.value.orderSerialNumber = choseOrderInfo.value.serialNumber
    form.value.basicClass = choseOrderInfo.value.basicClass
    form.value.basicClassName = choseOrderInfo.value.basicClassName
    form.value.supplierId = choseOrderInfo.value.supplierId
    form.value.supplierName = choseOrderInfo.value.supplierName
    form.value.invoiceUnit = choseOrderInfo.value.supplierName
    form.value.receiveInvoiceUnit = undefined
    form.value.receiveInvoiceUnitId = undefined
  } else {
    clearInfo()
  }
}

function orderCompanyChange(val) {
  if (val) {
    const orderVal = choseOrderInfo.value.companyBankAccountList.find((v) => v.companyId === val)
    form.value.receiveInvoiceUnit = orderVal.companyName
  } else {
    form.value.receiveInvoiceUnit = undefined
  }
}

function MoneyData() {
  if (isModify.value) {
    if (choseOrderInfo.value.amount && form.value.taxRate) {
      return ((choseOrderInfo.value.amount * form.value.taxRate) / 100).toFixed(DP.YUAN)
    }
  } else {
    if (choseOrderInfo.value.amount && props.collectionInfo.taxRate) {
      return ((choseOrderInfo.value.amount * props.collectionInfo.taxRate) / 100).toFixed(DP.YUAN)
    }
  }
}

const rateMoney = computed(() => {
  return MoneyData
})

async function onSubmit(val) {
  try {
    if (props.type === 'detail') {
      const valid = await formRef.value.validate()
      form.value.tax = rateMoney.value || ''
      form.value.attachmentIds = form.value.attachments ? form.value.attachments.map((v) => v.id) : undefined
      if (valid) {
        // 修改
      }
    } else {
      const submitData = {
        id: props.collectionInfo.id,
        auditStatus: val
      }
      await editStatus(submitData)
      ElNotification({ title: '提交成功', type: 'success' })
      emit('success')
      closeDrawer()
    }
  } catch (e) {
    console.log('收票信息', e)
  }
}
</script>
<style lang="scss" scoped>
::v-deep(.el-input-number .el-input__inner) {
  text-align: left;
}
</style>
