<template>
  <common-dialog
    append-to-body
    v-model="visible"
    top="10vh"
    width="60%"
    :before-close="handleClose"
    title="审核详情"
    :center="false"
    :close-on-click-modal="false"
  >
    <template #title>
      <div class="dialog-title">
        <span class="title-left">审核详情</span>
        <el-tag v-if="auditStatus" size="medium" :type="auditStatus==auditTypeEnum.REJECT.V?'info':(auditStatus==auditTypeEnum.PASS.V?'success':'warning')">
          {{ auditStatus==auditTypeEnum.REJECT.V?'已驳回':(auditStatus==auditTypeEnum.PASS.V?'已通过':'审核中') }}
        </el-tag>
        <span style="position:absolute;right:20px;">
          <common-button v-if="showType==='audit'" size="small" type="info" @click="passConfirm(auditTypeEnum.REJECT.V)">驳回</common-button>
          <common-button v-if="showType==='audit'" size="small" type="success" @click="passConfirm(auditTypeEnum.PASS.V)">通过</common-button>
          <common-button size="small"  @click="handleClose">关闭</common-button>
        </span>
      </div>
    </template>
    <el-descriptions class="margin-top" :column="2" border>
      <el-descriptions-item label-class-name="contractLabel" label="申请人">kooriookami</el-descriptions-item>
      <el-descriptions-item label-class-name="contractLabel" label="申请日期">2021/1/1</el-descriptions-item>
      <el-descriptions-item label-class-name="contractLabel" label="采购单号">CG吉利汽车新能源项目-G2221-01</el-descriptions-item>
      <el-descriptions-item label-class-name="contractLabel" label="所属项目">CMJK-22123-01吉利汽车新能源项目</el-descriptions-item>
      <el-descriptions-item label-class-name="contractLabel" label="供应商">杭州建航钢结构有限公司</el-descriptions-item>
      <el-descriptions-item label-class-name="contractLabel" label="合同额">20000</el-descriptions-item>
      <el-descriptions-item label-class-name="contractLabel" label="已付款">15,000 | 75%</el-descriptions-item>
      <el-descriptions-item label-class-name="contractLabel" label="最近一次付款">1,500 | 2022/1/1</el-descriptions-item>
      <el-descriptions-item label-class-name="contractLabel" label="本次申请">
        <el-tag effect="plain">1000</el-tag>
        <el-tag type="success" style="margin-left:5px;">5%</el-tag>
      </el-descriptions-item>
      <el-descriptions-item label-class-name="contractLabel" label="大写"><el-tag effect="plain">1000</el-tag></el-descriptions-item>
      <el-descriptions-item label-class-name="contractLabel" label="付款事由">进度款</el-descriptions-item>
      <el-descriptions-item label-class-name="contractLabel" label="收款单位">杭州建航钢结构有限公司</el-descriptions-item>
      <el-descriptions-item label-class-name="contractLabel" label="收款银行">中国银行西湖支行</el-descriptions-item>
      <el-descriptions-item label-class-name="contractLabel" label="账号">1234567</el-descriptions-item>
      <el-descriptions-item label-class-name="contractLabel" label="本次实付">
        <el-input-number
          v-model.number="actuallyPaymentAmount"
          v-show-thousand
          :min="0"
          :step="100"
          :precision="DP.YUAN"
          placeholder="本次实付(元)"
          controls-position="right"
          style="width:220px;"
          @change="moneyChange"
        />
        <!-- <el-input-number
          v-model.number="actuallyPaymentAmount"
          :min="0"
          :max="contractInfo.contractAmount"
          :step="100"
          :precision="DP.YUAN"
          placeholder="收款金额(元)"
          controls-position="right"
          @change="moneyChange"
        /> -->
        <el-tag type="success" style="margin-left:5px;">5%</el-tag>
      </el-descriptions-item>
      <el-descriptions-item label-class-name="contractLabel" label="付款银行">
        <common-select
          v-model="paymentBankId"
          :options="bankList"
          type="other"
          :dataStructure="typeProp"
          size="small"
          placeholder="收款银行"
          style="width:100%;"
        />
      </el-descriptions-item>
      <el-descriptions-item label-class-name="contractLabel" label="附件">
        <template v-if="detailInfo && detailInfo.attachmentList && detailInfo.attachmentList.length>0">
          <div v-for="item in detailInfo.attachmentList" :key="item.id">{{item.name}}
            <export-button :params="{id: item.id}"/>
          </div>
        </template></el-descriptions-item>
    </el-descriptions>
  </common-dialog>
</template>

<script setup>
import { ref, defineProps, watch, defineEmits, nextTick } from 'vue'
import { auditTypeEnum } from '@enum-ms/contract'
import useVisible from '@compos/use-visible'
import { confirmContract } from '@/api/contract/project'
import { bankData } from '@/api/contract/collection-and-invoice/collection'
import { DP } from '@/settings/config'
// import { isNotBlank } from '@data-type/index'
import { ElNotification, ElMessageBox } from 'element-plus'
// import { parseTime } from '@/utils/date'
// import useWatchFormValidate from '@compos/form/use-watch-form-validate'
// import { toThousand } from '@data-type/number'
import ExportButton from '@comp-common/export-button/index.vue'

const props = defineProps({
  projectId: [Number, String],
  auditStatus: [Number, String],
  modelValue: {
    type: Boolean,
    require: true
  },
  showType: {
    type: String,
    default: undefined
  },
  detailInfo: {
    type: Object,
    default: () => {}
  },
  memberList: {
    type: Array,
    default: () => []
  }
})

const defaultForm = {
  id: undefined,
  projectId: '',
  changeMoney: undefined,
  changeAmount: undefined,
  changeContent: undefined,
  changeDate: '',
  userList: [],
  changeDesc: undefined,
  attachments: undefined,
  attachmentIds: undefined
}

const bankList = ref([])
const typeProp = { key: 'id', label: 'depositBank', value: 'id' }
const form = ref(JSON.parse(JSON.stringify(defaultForm)))
const formRef = ref()
const actuallyPaymentAmount = ref()
const paymentBankId = ref()
const emit = defineEmits(['success', 'update:modelValue'])
const { visible, handleClose } = useVisible({ emit, props })

watch(
  () => visible.value,
  (val) => {
    if (val) {
      if (!props.auditStatus) {
        resetForm()
      }
    }
  },
  { deep: true, immediate: true }
)

function resetForm(data) {
  if (formRef.value) {
    formRef.value.resetFields()
  }
  if (data && Object.keys(data).length > 0) {
    form.value = data
  } else {
    form.value = JSON.parse(JSON.stringify(defaultForm))
  }
  if (formRef.value) {
    nextTick(() => {
      formRef.value.clearValidate()
    })
  }
}

const inputValid = (val) => {
  if ((!val || !val.trim()) && val !== 0) {
    return '必填'
  }
  if (val.length > 200) {
    return '长度在 1 到 200 个字符'
  }
  return true
}

async function getBankData(companyId) {
  try {
    const { content } = await bankData(companyId)
    bankList.value = content
  } catch (e) {
    console.log('获取银行账号', e)
  }
}

function moneyChange(val) {

}
// function bankChange(row) {
//   if (row.collectionBankAccountId) {
//     const choseVal = bankList.value.find(v => v.id === row.collectionBankAccountId)
//     row.collectionDepositBank = choseVal.depositBank
//     row.collectionBankAccount = choseVal.account
//   } else {
//     row.collectionDepositBank = undefined
//     row.collectionBankAccount = undefined
//   }
// }

async function passConfirm(val) {
  // console.log(actuallyPaymentAmount.value)
  try {
    const title = val === auditTypeEnum.PASS.V ? '通过' : '驳回'
    const remarkValue = await ElMessageBox.prompt('请输入审核说明', title, {
      confirmButtonText: '确定',
      cancelButtonText: '取消',
      inputType: 'textarea',
      inputValidator: inputValid,
      type: 'warning'
    })
    const submitData = {
      auditStatus: val,
      id: props.detailInfo.id,
      remark: remarkValue.value
    }
    await confirmContract(submitData)
    ElNotification({ title: '提交成功', type: 'success' })
    emit('success')
    handleClose()
  } catch (error) {
    console.log('审核', error)
  }
}
</script>

<style rel="stylesheet/scss" lang="scss" scoped>
  ::v-deep(.el-input-number .el-input__inner) {
    text-align: left;
  }
  .title-left {
    display: flex;
    align-items: center;
    position: relative;
    padding-left: 10px;
    margin-right: 15px;
    box-sizing: border-box;
  }
  .title-left::before {
    content: "";
    width: 4px;
    height: 15px;
    border-radius: 10px;
    background: #1890ff;
    position: absolute;
    top: 50%;
    left: 0;
    transform: translateY(-50%);
}
.tip-red{
  color:red;
}
.tip-green{
  color:#67c23a;
}
.detail-break{
  word-break:break-all;
}
</style>
