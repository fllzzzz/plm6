<template>
  <common-drawer
    append-to-body
    :close-on-click-modal="false"
    :before-close="crud.cancelCU"
    :visible="crud.status.cu > 0"
    title="新增付款申请"
    :wrapper-closable="false"
    size="50%"
  >
    <template #titleRight>
      <common-button :loading="crud.status.cu === 2" type="primary" size="mini" @click="crud.submitCU">提交审核</common-button>
    </template>
    <template #content>
      <el-form ref="formRef" :model="form" :rules="rules" size="small" label-width="120px" label-position="right">
      <el-divider><span class="title">金额汇总</span></el-divider>
        <common-table
          ref="detailRef"
          border
          :data="freightDetails"
          :max-height="maxHeight"
          style="width: 100%;margin-bottom:10px;"
          class="table-form"
          return-source-data
          :showEmptySymbol="false"
          v-loading="tableLoading"
          show-summary
          :summary-method="getSummaries"
        >
          <el-table-column key="projectName" prop="projectName" label="项目/采购订单" align="center" >
            <template v-slot="scope">
              <div>{{ scope.row.projectName || scope.row.serialNumber  }}</div>
            </template>
          </el-table-column>
          <el-table-column key="type" prop="type" label="运输属性" align="center" >
            <template v-slot="scope">
              <div>{{ scope.row.type? logisticsSearchTypeEnum.VL[scope.row.type]: '-' }}</div>
            </template>
          </el-table-column>
          <el-table-column key="freight" prop="freight" label="运费总额" align="center" min-width="120" :show-overflow-tooltip="true">
            <template v-slot="scope">
              <div>{{ toThousand(scope.row.freight) }}</div>
            </template>
          </el-table-column>
          <el-table-column key="paymentAmount" prop="paymentAmount" label="已支付" align="center" min-width="120" :show-overflow-tooltip="true">
            <template v-slot="scope">
              <div>{{ toThousand(scope.row.paymentAmount) }}</div>
            </template>
          </el-table-column>
           <el-table-column key="applyAmount" prop="applyAmount" label="本次支付金额(元)" align="center" min-width="120" :show-overflow-tooltip="true">
            <template v-slot="scope">
              <el-input-number
                v-if="scope.row.freight!==scope.row.paymentAmount"
                v-model.number="scope.row.applyAmount"
                v-show-thousand
                :min="0"
                :max="scope.row.freight-scope.row.paymentAmount"
                :step="100"
                :precision="DP.YUAN"
                placeholder="本次支付(元)"
                controls-position="right"
              />
            </template>
          </el-table-column>
        </common-table>
        <el-divider><span class="title">明细填报</span></el-divider>
        <el-form-item label="物流公司">
          <span>{{ detailInfo.supplierName }}</span>
        </el-form-item>
        <el-form-item label="已付款">
          <span v-thousand="detailInfo.paymentAmount"/><span>（{{ detailInfo.paymentRate }}%）</span>
        </el-form-item>
        <el-form-item label="已收票">
          <span v-thousand="detailInfo.invoiceAmount"/><span>（{{ detailInfo.invoiceRate }}%）</span>
        </el-form-item>
        <el-form-item label="最后一次付款">
          <span v-if="detailInfo.lastPaymentAmount" v-thousand="detailInfo.lastPaymentAmount" />
          <el-tag v-if="detailInfo.lastPaymentTime" style="margin-left:5px;">{{ parseTime(detailInfo.lastPaymentTime,'{y}-{m}-{d}')}}</el-tag>
        </el-form-item>
        <el-form-item label="本次付款">
          <span v-thousand="totalAmount" />
        </el-form-item>
        <el-form-item label="大写">
            <span style="color:#82848a">{{totalAmount?digitUppercase(totalAmount):''}}</span>
        </el-form-item>
        <el-form-item label="申请日期" prop="paymentDate">
          <el-date-picker
            v-model="form.paymentDate"
            type="date"
            value-format="x"
            placeholder="选择申请日期"
            :disabledDate="(date) => {return date.getTime() > new Date().getTime()}"
            style="width: 220px"
          />
        </el-form-item>
        <el-form-item label="开户行">
          <span>{{detailInfo.receiveBank}}</span>
        </el-form-item>
        <el-form-item label="银行账号">
          <span>{{detailInfo.receiveBankAccount}}</span>
        </el-form-item>
        <el-form-item label="备注" prop="remark">
          <el-input
            v-model="form.remark"
            type="textarea"
            style="width: 100%"
            maxlength="500"
            :autosize="{ minRows: 2, maxRows: 4 }"
            placeholder="请输入备注"
          />
        </el-form-item>
        <el-form-item label="附件">
          <upload-btn ref="uploadRef" v-model:files="form.attachments" :file-classify="fileClassifyEnum.CONTRACT_ATT.V" :limit="1" :accept="'.zip,.jpg,.png,.pdf,.jpeg'"/>
        </el-form-item>
      </el-form>
    </template>
  </common-drawer>
</template>

<script setup>
import { payableList } from '@/api/supply-chain/logistics-payment-manage/logistics-payment'
import { ref, computed, defineProps } from 'vue'

import moment from 'moment'
import { tableSummary } from '@/utils/el-extra'
import { toThousand } from '@data-type/number'
import { DP } from '@/settings/config'
import { fileClassifyEnum } from '@enum-ms/file'
import { logisticsSearchTypeEnum } from '@enum-ms/contract'
import { digitUppercase } from '@/utils/data-type/number'

import { regForm } from '@compos/use-crud'
import useMaxHeight from '@compos/use-max-height'
import UploadBtn from '@comp/file-upload/UploadBtn'
import { ElMessage } from 'element-plus'

// 获取数据源
const totalAmount = computed(() => {
  return freightDetails.value.reduce((prev, curr) => {
    const value = Number(curr?.applyAmount)
    if (!isNaN(value)) {
      return prev + value
    } else {
      return prev
    }
  }, 0)
})

const formRef = ref()
const props = defineProps({
  detailInfo: {
    type: Object,
    default: () => {}
  }
})
const defaultForm = {
  paymentDate: moment().startOf('day').format('x'), // 默认当天0点的时间戳
  applyAmount: undefined,
  attachmentIds: undefined,
  attachments: [],
  branchCompanyId: undefined,
  supplierId: undefined,
  detailSaveParams: [],
  remark: ''
}

const { maxHeight } = useMaxHeight({
  wrapperBox: '.paymentAddForm',
  paginate: true,
  extraHeight: 40
})

const { CRUD, crud, form } = regForm(defaultForm, formRef)

const validateMoney = (rule, value, callback) => {
  if (!value) {
    callback(new Error('请填写申请金额并大于0'))
  }
  callback()
}

const rules = {
  paymentDate: [{ required: true, message: '请选择申请日期', trigger: 'change' }],
  applyAmount: [{ required: true, validator: validateMoney, trigger: 'blur' }]
}

const tableLoading = ref(false)
const freightDetails = ref([])
// 获取可付款列表
async function fetchList() {
  let _list = []
  tableLoading.value = true
  try {
    const { content = [] } = await payableList({ supplierId: props.detailInfo.supplierId, branchCompanyId: props.detailInfo.branchCompanyId })
    _list = content
  } catch (error) {
    console.log('获取可付款列表失败', error)
  } finally {
    freightDetails.value = _list
    tableLoading.value = false
  }
}

// 合计
function getSummaries(param) {
  return tableSummary(param, {
    props: ['freight', 'paymentAmount', 'applyAmount'],
    toThousandFields: ['freight', 'paymentAmount', 'applyAmount']
  })
}

CRUD.HOOK.afterToAdd = () => {
  fetchList()
}

CRUD.HOOK.beforeSubmit = () => {
  crud.form.attachmentIds = crud.form.attachments ? crud.form.attachments.map((v) => v.id) : undefined
  crud.form.supplierId = props.detailInfo.supplierId || 1
  crud.form.branchCompanyId = props.detailInfo.branchCompanyId || 1
  crud.form.applyAmount = 0
  crud.form.detailSaveParams = []
  freightDetails.value.map(v => {
    if (v.applyAmount > 0) {
      crud.form.applyAmount += v.applyAmount
      crud.form.detailSaveParams.push({
        projectId: v.projectId,
        purchaseId: v.purchaseId,
        type: v.type,
        applyAmount: v.applyAmount
      })
    }
  })
  if (crud.form.detailSaveParams.length === 0) {
    ElMessage.error('请填写本次申请明细且金额大于0')
    return false
  }
}
</script>
<style lang="scss" scoped>
.add-row-box {
  text-align: center;
  margin-top: 20px;
}
</style>
