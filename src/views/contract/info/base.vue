<template>
  <div id="baseContainer" class="app-container">
    <el-form
      ref="formRef"
      :model="form"
      :rules="rules"
      inline
      size="small"
      label-position="right"
      label-width="104px"
    >
      <div>
        <div class="form-row">
          <el-form-item label="合同编号" prop="serialNumber">
            <div class="input-underline">
              <el-input
                v-if="isModify"
                v-model.trim="form.serialNumber"
                placeholder="合同编号"
                style="width:260px;"
              />
              <span v-else>{{ detail.serialNumber || '-' }}</span>
            </div>
          </el-form-item>
          <el-form-item label="项目名称" prop="name">
            <div class="input-underline">
              <el-input
                v-if="isModify"
                v-model.trim="form.name"
                placeholder="项目名称"
                style="width:320px;"
              />
              <span v-else>{{ detail.name || '-' }}</span>
            </div>
          </el-form-item>
          <el-form-item label="订单来源" prop="orderSourceType">
            <div class="input-underline">
              <common-select
                v-if="isModify"
                v-model="form.orderSourceType"
                :options="orderSourceTypeEnum.ENUM"
                type="enum"
                class="input-underline"
                size="small"
                placeholder="订单来源"
                style="width: 200px"
              />
              <span v-else>{{detail.orderSourceType?orderSourceTypeEnum.VL[detail.orderSourceType]:'-'}}</span>
            </div>
          </el-form-item>
        </div>
        <div>
          <el-form-item label="开工时间" prop="startDate">
            <div class="input-underline">
              <el-date-picker
                v-if="isModify"
                v-model="form.startDate"
                type="date"
                class="input-underline"
                value-format="x"
                placeholder="选择约定开工日期"
                style="width:260px"
              />
              <template v-else>
                <span>{{ detail.startDate? parseTime(detail.startDate,'{y}-{m}-{d}'): '-' }}</span>
              </template>
            </div>
          </el-form-item>
          <el-form-item label="项目简称" prop="shortName">
            <div class="input-underline">
              <el-input
                v-if="isModify"
                v-model.trim="form.shortName"
                placeholder="项目简称"
                style="width:320px;"
                maxlength="12"
              />
              <span v-else>{{ detail.shortName || '-' }}</span>
            </div>
          </el-form-item>
        </div>
        <div class="form-row">
          <el-form-item label="完成时间" prop="endDate">
            <div class="input-underline">
              <el-date-picker
                v-if="isModify"
                v-model="form.endDate"
                type="date"
                class="input-underline"
                value-format="x"
                placeholder="选择约定完成日期"
                style="width:260px"
                :disabledDate="endDateOption"
              />
              <template v-else>
                <span>{{ detail.endDate? parseTime(detail.endDate,'{y}-{m}-{d}'): '-' }}</span>
              </template>
            </div>
          </el-form-item>
          <el-form-item label="总工期(天)" prop="totalDuration">
            <div class="input-underline">
              <span v-if="isModify">{{ totalDuration }}</span>
              <span v-else>{{ detail.totalDuration || '-' }}</span>
            </div>
          </el-form-item>
        </div>
        <div class="form-row">
          <el-form-item label="项目省市区" prop="region">
            <div class="input-underline">
              <region-cascader
                v-if="isModify"
                class="input-underline"
                ref="region"
                style="width:200px"
                v-model="form.region"
                clearable
                filterable
                @change="handleRegionChange"
              />
              <span v-else>{{ detail.regionalFullName || '-' }}</span>
            </div>
          </el-form-item>
          <el-form-item label="详细地址" prop="address">
            <div class="input-underline" style="width:420px">
              <el-input
                v-if="isModify"
                v-model.trim="form.address"
                placeholder="项目详细地址"
              />
              <span v-else class="detail-break">{{ detail.address || '-' }}</span>
            </div>
          </el-form-item>
        </div>
        <el-divider><span class="title">负责人</span></el-divider>
        <div class="form-row">
          <el-form-item label="项目经理" prop="projectManagerId">
            <div class="input-underline" style="width:300px">
              <user-dept-cascader
                v-if="isModify"
                v-model="form.projectManagerId"
                filterable
                :collapse-tags="false"
                clearable
                class="input-underline"
                style="width:220px"
                placeholder="项目经理"
              />
              <span v-else>{{ detail.projectManagerFullName || '-' }}</span>
            </div>
          </el-form-item>
          <el-form-item label="销售负责人" prop="signerId">
            <user-dept-cascader
              v-if="isModify"
              v-model="form.signerId"
              filterable
              :collapse-tags="false"
              clearable
              class="input-underline"
              style="width: 200px"
              placeholder="销售负责人"
            />
            <span v-else>{{ detail.signerName || '-' }}</span>
          </el-form-item>
        </div>
        <el-divider><span class="title">合同金额</span></el-divider>
        <div class="form-row">
          <el-form-item label="合同金额(元)" prop="contractAmount">
            <div class="input-underline">
              <span>{{ detail.contractAmount? toThousand(detail.contractAmount,decimalPrecision.contract): '-' }}</span>
              <div style="color:#82848a">{{ detail.contractAmount? digitUppercase(detail.contractAmount):'' }}</div>
            </div>
          </el-form-item>
          <el-form-item label="预付款(元)" prop="prepayments">
            <div class="input-underline">
              <el-input-number
                v-show-thousand
                v-if="isModify"
                v-model="form.prepayments"
                :step="1"
                :min="0"
                :max="detail.contractAmount?detail.contractAmount:999999999999"
                :precision="decimalPrecision.contract"
                :controls="false"
                controls-position="right"
                placeholder="预付款(元)"
                style="width:220px;"
              />
              <template v-else>
                <span>{{ detail.prepayments? toThousand(detail.prepayments,decimalPrecision.contract): '' }}</span>
              </template>
            </div>
          </el-form-item>
          <el-form-item label="管理费(元)" prop="managementFeeRate">
            <template v-if="isModify">
              <div class="input-underline" style="display:inline-block;width:110px">
                <el-input
                  v-model="managementFee"
                  :readonly="true"
                  placeholder="先输入费率"
                />
              </div>
              <div class="input-underline" style="display:inline-block;width:130px">
                <el-input-number
                  v-model="form.managementFeeRate"
                  :step="1"
                  :min="0"
                  :max="100"
                  :precision="3"
                  :controls="false"
                  controls-position="right"
                  placeholder="0-100"
                  style="width:80px"
                />%
              </div>
            </template>
            <template v-else>
              <span>{{ detail.managementFee? toThousand(detail.managementFee,decimalPrecision.contract): '-' }}</span>
              <span>（费率:{{ detail.managementFeeRate ? detail.managementFeeRate.toFixed(DP.ACCOUNTING): '-' }}%）</span>
            </template>
          </el-form-item>
        </div>
        <div class="form-row">
          <el-form-item label="保证金(元)" prop="marginAmount">
            <div class="input-underline">
              <el-input-number
                v-if="isModify"
                v-model="form.marginAmount"
                :step="1"
                :min="0"
                :max="999999999999"
                :precision="decimalPrecision.contract"
                :controls="false"
                controls-position="right"
                placeholder="保证金(元)"
                style="width:220px;"
              />
              <template v-else>
                <span>{{ detail.marginAmount? toThousand(detail.marginAmount,decimalPrecision.contract): '-' }}</span>
              </template>
            </div>
          </el-form-item>
          <el-form-item label="保证金类型" prop="marginType">
            <div class="input-underline">
              <common-select
                v-if="isModify"
                v-model="form.marginType"
                :options="dict.margin_type"
                type="dict"
                size="small"
                clearable
                placeholder="保证金类型"
                class="input-underline"
                style="width:220px;"
              />
              <template v-else>
                <span>{{ detail.marginType && dict && dict.label && dict.label['margin_type']? dict.label['margin_type'][detail.marginType]: '-' }}</span>
              </template>
            </div>
          </el-form-item>
          <el-form-item label="币种" prop="currencyType">
            <div class="input-underline">
              <common-select
                v-if="isModify"
                v-model="form.currencyType"
                :options="dict.currency_type"
                type="dict"
                size="small"
                clearable
                placeholder="币种"
                class="input-underline"
                style="width:220px;"
              />
              <template v-else>
                <span>{{ detail.currencyType && dict && dict.label && dict.label['currency_type']? dict.label['currency_type'][detail.currencyType]: '-'}}</span>
              </template>
            </div>
          </el-form-item>
        </div>
      </div>
      <el-divider><span class="title">合同附件</span></el-divider>
      <div class="table-box">
        <upload-list
          v-if="!isModify"
          showView
          show-download
          :file-classify="fileClassifyEnum.CONTRACT_ATT.V"
          v-model:files="detail.attachmentFiles"
          :download-fn="downloadBaseAttachments"
          :uploadable="isModify"
          empty-text="暂未上传合同附件"
        />
        <upload-list
          v-else
          showView
          show-download
          :file-classify="fileClassifyEnum.CONTRACT_ATT.V"
          v-model:files="form.attachmentFiles"
          :download-fn="downloadBaseAttachments"
          :uploadable="isModify"
          empty-text="暂未上传合同附件"
        />
      </div>
    </el-form>
  </div>
</template>

<script setup>
import { ref, defineProps, watch, computed, defineExpose, nextTick } from 'vue'

import { dateDifference } from '@/utils/date'
import { cleanArray } from '@data-type/array'
import useDict from '@compos/store/use-dict'
import { fileClassifyEnum } from '@enum-ms/file'
import { orderSourceTypeEnum } from '@enum-ms/contract'
import useWatchFormValidate from '@compos/form/use-watch-form-validate'
import { DP } from '@/settings/config'
import { getContractBase, downloadBaseAttachments } from '@/api/contract/project'
import { parseTime } from '@/utils/date'
import { digitUppercase, toThousand } from '@/utils/data-type/number'
import useDecimalPrecision from '@compos/store/use-decimal-precision'

import regionCascader from '@comp-base/region-cascader'
import userDeptCascader from '@comp-base/user-dept-cascader.vue'
import uploadList from '@comp/file-upload/UploadList.vue'

const { decimalPrecision } = useDecimalPrecision()

const formRef = ref()
const dict = useDict(['margin_type', 'currency_type'])
const defaultForm = {
  id: undefined,
  serialNumber: undefined, // 合同编号
  name: undefined, // 项目名称
  shortName: undefined, // 项目简称
  region: [], // 项目省市区
  countryId: undefined, // 国家
  provinceId: undefined, // 省
  cityId: undefined, // 市
  regionId: undefined, // 区
  address: undefined, // 项目详细地址
  startDate: undefined, // 项目开始时间
  endDate: undefined, // 项目结束时间
  contractAmount: undefined, // 合同金额
  prepayments: undefined, // 预付款
  managementFeeRate: undefined, // 管理费率
  marginAmount: undefined, // 保证金额
  marginType: undefined, // 保证金类型
  currencyType: undefined, // 币种
  projectManagerId: undefined, // 项目经理
  businessLeaderId: undefined, // 业务负责人1
  businessLeaderTwoId: undefined, // 业务负责人2
  attachmentFiles: [], // 附件
  attachments: [],
  orderSourceType: undefined,
  signerId: undefined // 销售负责人
}

const form = ref(JSON.parse(JSON.stringify(defaultForm)))
const detail = ref(JSON.parse(JSON.stringify(defaultForm)))
const validateMoney = (rule, value, callback) => {
  if (!value) {
    callback(new Error('合同金额必须大于0'))
  } else {
    callback()
  }
}
const rules = {
  serialNumber: [
    { required: true, message: '请填写合同编号', trigger: 'blur' },
    { min: 1, max: 60, message: '长度在 1 到 60 个字符', trigger: 'blur' }
  ],
  name: [
    { required: true, message: '请填写项目名称', trigger: 'blur' },
    { min: 1, max: 60, message: '长度在 1 到 60 个字符', trigger: 'blur' }
  ],
  shortName: [
    { required: true, message: '请填写项目简称', trigger: 'blur' },
    { min: 1, max: 12, message: '长度在 1 到 12 个字符', trigger: 'blur' }
  ],
  orderSourceType: [{ required: true, message: '请选择订单来源', trigger: 'change' }],
  startDate: [{ required: true, message: '请选择开工日期', trigger: 'change' }],
  endDate: [{ required: true, message: '请选择完工日期', trigger: 'change' }],
  contractAmount: [{ required: true, validator: validateMoney, trigger: 'blur' }],
  address: [
    { max: 200, message: '长度不超过 200 个字符', trigger: 'blur' }
  ],
  signingAddress: [
    { max: 200, message: '长度不超过 200 个字符', trigger: 'blur' }
  ]
}

const props = defineProps({
  projectId: {
    type: [Number, String],
    default: undefined
  },
  isModify: {
    type: Boolean,
    default: false
  }
})

watch(
  () => props.projectId,
  (val) => {
    fetchDetail()
  },
  { deep: true, immediate: true }
)

// watch(
//   () => props.isModify,
//   (val) => {
//     resetForm()
//   },
//   { deep: true, immediate: true }
// )

const totalDuration = computed(() => {
  if (form.value.startDate && form.value.endDate) {
    return dateDifference(form.value.startDate, form.value.endDate)
  }
  return ''
})
const managementFee = computed(() => {
  if (form.value.managementFeeRate && form.value.contractAmount) {
    return (form.value.managementFeeRate * form.value.contractAmount / 100).toFixed(decimalPrecision.value.contract)
  }
  return undefined
})
/**
 * 重置表单
 */
function resetForm() {
  if (formRef.value) {
    formRef.value.resetFields()
  }
  form.value = JSON.parse(JSON.stringify(detail.value))
  if (formRef.value) {
    nextTick(() => {
      formRef.value.clearValidate()
    })
  }
  useWatchFormValidate(formRef, form)
}

async function validateForm() {
  try {
    const valid = await formRef.value.validate()
    return valid
  } catch (error) {
    console.log('error', error)
    return false
  }
}

function endDateOption(time) {
  if (form.value.startDate) {
    return time.getTime() - 8.64e6 < form.value.startDate
  } else {
    return time.getTime() - 8.64e6 < form.value.createTime
  }
}

function handleRegionChange(val) {
  form.value.countryId = undefined
  form.value.provinceId = undefined
  form.value.cityId = undefined
  form.value.regionId = undefined
  val && val.forEach((v, i) => {
    if (i === 0) {
      form.value.countryId = v
    }
    if (i === 1) {
      form.value.provinceId = v
    }
    if (i === 2) {
      form.value.cityId = v
    }
    if (i === 3) {
      form.value.regionId = v
    }
  })
}

async function fetchDetail() {
  if (!props.projectId) {
    return
  }
  let _detail = {}
  try {
    const res = await getContractBase(props.projectId)
    _detail = JSON.parse(JSON.stringify(res))
    _detail.startDate = _detail.startDate ? String(_detail.startDate) : ''
    _detail.endDate = _detail.endDate ? String(_detail.endDate) : ''
    _detail.totalDuration = _detail.startDate && _detail.endDate ? dateDifference(_detail.startDate, _detail.endDate) : ''
    _detail.managementFee = _detail.managementFeeRate && _detail.contractAmount ? _detail.managementFeeRate * _detail.contractAmount / 100 : ''
    _detail.attachments = _detail.attachments || []
    _detail.attachmentFiles = _detail.attachments
    _detail.region = cleanArray([_detail.countryId, _detail.provinceId, _detail.cityId, _detail.regionId])
  } catch (error) {
    console.log('error', error)
  } finally {
    detail.value = _detail
    resetForm(detail.value)
  }
}

defineExpose({
  detail,
  form,
  validateForm,
  fetchDetail,
  resetForm
})
</script>
<style lang="scss" scoped>
.app-container{
  position: relative;
  .operate-btn {
    position: absolute;
    right: 50px;
    top: 20px;
  }
}
.table-box {
  box-sizing: border-box;
  padding: 0 25px;
}
::v-deep(.el-input-number .el-input__inner) {
  text-align: left;
}
::v-deep(.input-underline) {
  // width: calc((95vw - 40px)/3);
  width: 320px;
  margin-right: 0;
  input{
    border-top:0;
    border-left:0;
    border-right:0;
    border-radius: 0;
  }
}
.form-row {
  width:100%
}
span {
  // color:#4482ff #1682e6
  color:#82848a
}
.detail-break{
  word-break:break-all;
}
</style>
