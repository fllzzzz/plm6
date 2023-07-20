<template>
  <common-drawer
    ref="drawerRef"
    append-to-body
    :close-on-click-modal="false"
    :before-close="closeDialog"
    v-model="visible"
    title="付款申请详情"
    :wrapper-closable="false"
    size="85%"
  >
    <template #titleAfter>
      <el-tag v-if="currentRow.auditStatus" size="medium" :type="currentRow.auditStatus===auditTypeEnum.REJECT.V?'warning':(currentRow.auditStatus===auditTypeEnum.PASS.V?'success':'')">
        {{ currentRow.auditStatus===auditTypeEnum.REJECT.V?'已驳回':(currentRow.auditStatus===auditTypeEnum.PASS.V?'已通过':'审核中') }}
      </el-tag>
    </template>
    <template #titleRight>
      <template v-if="showType==='audit' && currentRow.auditStatus===auditTypeEnum.AUDITING.V">
        <common-button size="small" type="info" @click="passConfirm(auditTypeEnum.ENUM.REJECT.V)">驳回</common-button>
        <common-button size="small" type="success" @click="passConfirm(auditTypeEnum.ENUM.PASS.V)">通过</common-button>
      </template>
    </template>
    <template #content>
      <div class="detail-header">
        <el-form ref="formRef" size="small" label-width="130px" style="position:relative;">
          <el-row>
            <el-col :span="8">
              <el-form-item label="供应商">
                <span>{{ detailInfo.supplierName }}</span>
              </el-form-item>
            </el-col>
            <el-col :span="8">
              <el-form-item label="累计运输费">
                <span>{{toThousand(totalFreight)}}</span>
              </el-form-item>
            </el-col>
            <el-col :span="8">
              <el-form-item label="累计付款">
                <span>{{toThousand(detailInfo.paymentAmount)}}</span><span>（{{ (detailInfo.paymentRate).toFixed(2) }}%）</span>
              </el-form-item>
            </el-col>
          </el-row>
          <el-row>
            <el-col :span="8">
              <el-form-item label="实际收款单位">
                <span>{{currentRow.actualReceivingUnit}}</span>
              </el-form-item>
            </el-col>
            <el-col :span="8">
              <el-form-item label="累计收票">
                <span v-thousand="detailInfo.invoiceAmount"/><span>（{{ (detailInfo.invoiceRate).toFixed(2) }}%）</span>
              </el-form-item>
            </el-col>
            <el-col :span="8">
              <el-form-item label="最后一次付款">
                <span v-thousand="detailInfo.lastPaymentAmount" />
                <el-tag v-if="detailInfo.lastPaymentTime" style="margin-left:5px;">{{ parseTime(detailInfo.lastPaymentTime,'{y}-{m}-{d}')}}</el-tag>
              </el-form-item>
            </el-col>
          </el-row>
          <el-row>
            <el-col :span="8">
              <el-form-item label="开户行" prop="receiveBank">
                <span>{{currentRow.receiveBank || '-'}}</span>
              </el-form-item>
            </el-col>
            <el-col :span="8">
              <el-form-item label="本次付款"  prop="applyAmount">
                <span v-thousand="currentRow.applyAmount"/>
              </el-form-item>
            </el-col>
            <el-col :span="8">
              <el-form-item label="付款日期" prop="paymentDate">
                <span v-if="currentRow.paymentDate" style="margin-left:5px;">{{ parseTime(currentRow.paymentDate,'{y}-{m}-{d}')}}</span>
              </el-form-item>
            </el-col>
          </el-row>
          <el-row>
            <el-col :span="8">
              <el-form-item label="账号" prop="receiveBankAccount">
                <span>{{currentRow.receiveBankAccount || '-'}}</span>
              </el-form-item>
            </el-col>
            <el-col :span="8">
              <el-form-item label="大写">
                <span style="color:#82848a">{{currentRow?.sourceRow?.applyAmount?digitUppercase(currentRow?.sourceRow?.applyAmount):''}}</span>
                <div v-if="currentRow?.sourceRow?.applyAmount<totalFreight" style="color:red;">*让利金额<span v-thousand="totalFreight-currentRow?.sourceRow?.applyAmount" />元</div>
              </el-form-item>
            </el-col>
            <el-col :span="8">
              <el-form-item label="付款事由" prop="paymentReasonId">
                <div>{{ currentRow.paymentReasonId && dict && dict.label && dict.label['payment_reason']? dict.label['payment_reason'][ currentRow.paymentReasonId]: '' }}</div>
              </el-form-item>
            </el-col>
          </el-row>
          <el-row>
            <el-col :span="8">
              <el-form-item label="付款单位" prop="paymentUnitId">
                <span>{{currentRow.paymentUnit}}</span>
              </el-form-item>
            </el-col>
            <el-col :span="8">
              <el-form-item label="付款行" prop="paymentBank">
                <el-select v-if="showType==='audit' && currentRow.auditStatus===auditTypeEnum.AUDITING.V" v-model="paymentBank" placeholder="付款行" :size="'small'" style="width: 220px" @change="bankChange">
                  <el-option v-for="item in bankList" :key="item.account" :label="item.depositBank" :value="item.depositBank" />
                </el-select>
                <span v-else>{{currentRow.paymentBank}}</span>
              </el-form-item>
            </el-col>
            <el-col :span="8">
              <el-form-item label="账号">
                <span>{{paymentBankAccount}}</span>
              </el-form-item>
            </el-col>
          </el-row>
          <el-row>
            <el-col :span="8">
              <el-form-item label="操作人">
                <span>{{currentRow.applyUserName}}{{parseTime(currentRow.createTime,'{y}-{m}-{d}')}}</span>
              </el-form-item>
            </el-col>
            <el-col :span="8">
              <el-form-item label="审核人">
                <span>{{currentRow.auditUserName}}{{currentRow.auditTime?parseTime(currentRow.auditTime,'{y}-{m}-{d}'):''}}</span>
              </el-form-item>
            </el-col>
            <el-col :span="8">
              <el-form-item label="备注" prop="remark">
                <span>{{currentRow.remark || '-'}}</span>
              </el-form-item>
            </el-col>
          </el-row>
          <el-row>
            <el-col :span="8">
              <el-form-item label="付款凭证">
                <template #label>
                  付款凭证
                  <el-tooltip
                    effect="light"
                    :content="`双击可预览附件`"
                    placement="top"
                    v-if="currentRow.attachments?.length"
                  >
                    <i class="el-icon-info" />
                  </el-tooltip>
                </template>
                <template v-if="props.currentRow.attachments?.length">
                  <div v-for="item in currentRow.attachments" :key="item.id">
                    <div style="cursor:pointer;color:#409eff;" @dblclick="attachmentView(item)">{{item.name}}</div>
                  </div>
                </template>
              </el-form-item>
            </el-col>
          </el-row>
          <div v-if="currentRow.auditStatus===auditTypeEnum.PASS.V" style="position:absolute;top:50%;left:50%;transform:translateX(-50%) translateY(-50%);">
            <span style="font-size:26px;color:red;padding:10px 50px;border:1px solid red;">已支付</span>
          </div>
        </el-form>
        <el-divider><span class="title">支付物流明细</span></el-divider>
      </div>
      <common-table
        ref="detailRef"
        border
        :data="list"
        :max-height="maxHeight"
        style="width: 100%;margin-bottom:10px;"
        class="table-form"
        v-loading="tableLoading"
        :dataFormat="dataFormat"
      >
        <el-table-column key="purchaseSn" prop="purchaseSn" label="采购合同编号" align="center">
          <template v-slot="scope">
            <span>{{scope.row.purchaseSn}}</span>
          </template>
        </el-table-column>
        <el-table-column key="branchCompanyName" prop="branchCompanyName" label="合同签订主体" align="center" min-width="150" />
        <el-table-column key="purchaseUserName" prop="purchaseUserName" label="采购员" align="center" width="100" />
        <el-table-column prop="inboundSn" label="入库单号" align="center" show-overflow-tooltip>
          <template #default="{ row }">
            <span class="clickable" @click="openRecord(row)"> {{ row.inboundSn }}</span>
          </template>
        </el-table-column>
        <el-table-column key="freight" prop="freight" label="运输费" align="center" :show-overflow-tooltip="true" />
        <el-table-column key="licensePlate" prop="licensePlate" label="车牌号" align="center" :show-overflow-tooltip="true" />
        <el-table-column key="applicantName" prop="applicantName" label="入库人" align="center" :show-overflow-tooltip="true" width="100" />
      </common-table>
      <showPdfAndImg v-if="pdfShow" :isVisible="pdfShow" :showType="'attachment'" :id="currentId" @close="pdfShow=false"/>
      <inbound-record v-model="recordVisible" :detailInfo="currentRecordRow" />
    </template>
  </common-drawer>
</template>

<script setup>
import { logisticsIsPaymentList } from '@/api/contract/supplier-manage/jd-logistics-payment'
import { audit } from '@/api/contract/supplier-manage/jd-logistics-payment'
import { bankData } from '@/api/contract/collection-and-invoice/collection'
import { ref, defineProps, defineEmits, watch, nextTick, computed } from 'vue'
import { ElMessageBox, ElNotification } from 'element-plus'

import useMaxHeight from '@compos/use-max-height'
import { isBlank, isNotBlank } from '@/utils/data-type'
import { toThousand, digitUppercase } from '@data-type/number'
import { parseTime } from '@/utils/date'
import useVisible from '@compos/use-visible'
import useDict from '@compos/store/use-dict'
import { auditTypeEnum } from '@enum-ms/contract'
import showPdfAndImg from '@comp-base/show-pdf-and-img.vue'
import useDecimalPrecision from '@compos/store/use-decimal-precision'

import inboundRecord from '../inbound-record'

const { decimalPrecision } = useDecimalPrecision()

const emit = defineEmits(['success', 'update:modelValue'])
const { visible, handleClose } = useVisible({ emit, props })

const drawerRef = ref()
const formRef = ref()
const dict = useDict(['payment_reason'])
const props = defineProps({
  detailInfo: {
    type: Object,
    default: () => {}
  },
  currentRow: {
    type: Object,
    default: () => {}
  },
  showType: {
    type: String,
    default: 'detail'
  }
})

const pdfShow = ref(false)
const currentId = ref()
const paymentBank = ref()
const paymentBankAccount = ref()
const bankList = ref([])
const recordVisible = ref(false)
const currentRecordRow = ref({})

const detailRef = ref()
const list = ref([])
const tableLoading = ref(false)

const totalFreight = computed(() => {
  const freightData = list.value?.map((v) => v.freight)
  return freightData?.reduce((prev, curr) => {
    return prev + (curr || 0)
  }, 0) || 0
})

const dataFormat = ref([
  ['freight', ['to-thousand', decimalPrecision.contract]]
])

watch(
  () => props.currentRow,
  (val) => {
    if (val) {
      paymentBank.value = props.currentRow.paymentBank
      paymentBankAccount.value = props.currentRow.paymentBankAccount
      fetchBank()
      fetchList()
    }
  },
  { deep: true, immediate: true }
)

const { maxHeight } = useMaxHeight(
  {
    extraBox: ['.el-drawer__header', '.detail-header'],
    wrapperBox: ['.el-drawer__body', '.table-form'],
    navbar: false,
    extraHeight: 90
  },
  () => drawerRef.value.loaded
)

async function fetchList() {
  const _list = []
  tableLoading.value = true
  if (isBlank(props.currentRow)) {
    return
  }
  try {
    const { content = [] } = await logisticsIsPaymentList({ supplierId: props.detailInfo.supplierId, supplierPaymentId: props.currentRow.id })
    if (props.currentRow?.logisticsCargoIds.length > 0) {
      for (let i = 0; i < props.currentRow.logisticsCargoIds.length; i++) {
        const findVal = content.find(v => v.id === props.currentRow.logisticsCargoIds[i])
        if (isNotBlank(findVal)) {
          _list.push(findVal)
        }
      }
    }
  } catch (error) {
    console.log('获取物流付款记录失败', error)
  } finally {
    list.value = _list
    tableLoading.value = false
  }
}

async function fetchBank() {
  bankList.value = []
  if (!props.currentRow?.paymentUnitId) {
    return
  }
  try {
    const { content } = await bankData(props.currentRow.paymentUnitId)
    bankList.value = content || []
  } catch (e) {
    console.log('获取银行列表', e)
  }
}

// 打开入库记录
function openRecord(row) {
  currentRecordRow.value = row.sourceRow
  nextTick(() => {
    recordVisible.value = true
  })
}

// 预览附件
function attachmentView(item) {
  currentId.value = item.id
  pdfShow.value = true
}

function bankChange(val) {
  const findVal = bankList.value.find(v => v.depositBank === val) || {}
  paymentBankAccount.value = findVal.account
}

function closeDialog() {
  paymentBank.value = props.currentRow.paymentBank
  paymentBankAccount.value = props.currentRow.paymentBankAccount
  handleClose()
}

async function passConfirm(val) {
  try {
    const title = val === auditTypeEnum.PASS.V ? '通过' : '驳回'
    await ElMessageBox.confirm('该操作为最终审核,确认后将无法撤回', title, {
      confirmButtonText: '是',
      cancelButtonText: '否',
      type: 'warning'
    })
    await audit({ id: props.currentRow.id, auditStatus: val, paymentBank: paymentBank.value, paymentBankAccount: paymentBankAccount.value })
    ElNotification({ title: title + '成功', type: 'success' })
    handleClose()
    emit('success')
  } catch (e) {
    console.log('审核失败', e)
  }
}

</script>
<style lang="scss" scoped>
.add-row-box {
  text-align: center;
  margin-top: 20px;
}
</style>
