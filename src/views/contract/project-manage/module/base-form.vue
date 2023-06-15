<template>
  <el-form ref="formRef" :model="form" :rules="rules" inline size="small" label-position="right" label-width="104px" class="form-margin">
    <div>
      <div>
        <el-form-item label="合同编号" prop="serialNumber">
          <el-input v-model.trim="form.serialNumber" class="input-underline" placeholder="合同编号" style="width: 260px" />
        </el-form-item>
        <el-form-item label="项目名称" prop="name">
          <el-input v-model.trim="form.name" class="input-underline" placeholder="项目名称" style="width: 320px" />
        </el-form-item>
        <el-form-item label="订单来源" prop="orderSourceType">
          <common-select
            v-model="form.orderSourceType"
            :options="orderSourceTypeEnum.ENUM"
            type="enum"
            class="input-underline"
            size="small"
            placeholder="订单来源"
            style="width: 200px"
          />
        </el-form-item>
      </div>
      <div>
        <el-form-item label="开工时间" prop="startDate">
          <el-date-picker
            v-model="form.startDate"
            type="date"
            class="input-underline"
            value-format="x"
            placeholder="选择约定开工日期"
            style="width: 260px"
          />
        </el-form-item>
        <el-form-item label="项目简称" prop="shortName">
          <el-input v-model.trim="form.shortName" class="input-underline" placeholder="项目简称"  style="width: 320px" maxlength="12"/>
        </el-form-item>
      </div>
      <div class="form-row">
        <el-form-item label="完工日期" prop="endDate">
          <el-date-picker
            v-model="form.endDate"
            type="date"
            class="input-underline"
            value-format="x"
            placeholder="选择约定完成日期"
            style="width: 260px"
            :disabledDate="endDateOption"
          />
        </el-form-item>
        <el-form-item label="总工期(天)" prop="totalDuration">
          <span v-if="form.startDate && form.endDate">{{ dateDifference(form.startDate, form.endDate) }}</span>
        </el-form-item>
      </div>
      <div class="form-row">
        <el-form-item label="项目省市区" prop="region">
          <region-cascader
            class="input-underline"
            ref="region"
            style="width: 260px"
            v-model="form.region"
            clearable
            filterable
            @change="handleRegionChange"
          />
        </el-form-item>
        <el-form-item label="详细地址" prop="address">
          <el-input v-model.trim="form.address" placeholder="项目详细地址" class="input-underline" style="width: 420px" maxlength="200"/>
        </el-form-item>
      </div>
      <el-divider><span class="title">负责人</span></el-divider>
      <div class="form-row">
        <el-form-item label="项目经理" prop="projectManagerId">
          <user-dept-cascader
            v-model="form.projectManagerId"
            filterable
            :collapse-tags="false"
            clearable
            class="input-underline"
            style="width: 220px"
            placeholder="项目经理"
          />
        </el-form-item>
        <el-form-item label="销售负责人" prop="signerId">
          <user-dept-cascader
            v-model="form.signerId"
            filterable
            :collapse-tags="false"
            clearable
            class="input-underline"
            style="width: 200px"
            placeholder="销售负责人"
          />
        </el-form-item>
      </div>
      <el-divider><span class="title">合同金额</span></el-divider>
      <div class="form-row">
        <el-form-item label="合同金额(元)" prop="contractAmount">
          <div style="width:280px">
            <el-input-number
              v-show-thousand
              v-model="form.contractAmount"
              :step="1"
              :min="0"
              :max="999999999999"
              :precision="decimalPrecision.contract"
              :controls="false"
              controls-position="right"
              class="input-underline"
              style="width: 220px"
              placeholder="合同金额(元)"
            />
            <div style="color:#82848a">{{form.contractAmount?digitUppercase(form.contractAmount):''}}</div>
          </div>
        </el-form-item>
        <el-form-item label="预付款(元)" prop="prepayments">
          <el-input-number
            v-show-thousand
            v-model="form.prepayments"
            :step="1"
            :min="0"
            :max="form.contractAmount?form.contractAmount:999999999999"
            :precision="decimalPrecision.contract"
            :controls="false"
            controls-position="right"
            class="input-underline"
            style="width: 220px"
            placeholder="预付款(元)"
          />
        </el-form-item>
        <el-form-item label="管理费(元)" prop="managementFeeRate">
          <el-input v-model="managementFee" class="input-underline" :readonly="true" style="width: 110px" placeholder="先输入费率" />
          <el-input-number
            v-model="form.managementFeeRate"
            :step="1"
            :min="0"
            :max="100"
            :precision="DP.ACCOUNTING"
            :controls="false"
            controls-position="right"
            class="input-underline"
            style="width: 80px"
            placeholder="0-100"
          />%
        </el-form-item>
      </div>
      <div class="form-row">
        <el-form-item label="保证金(元)" prop="marginAmount">
          <div style="width:280px">
            <el-input-number
              v-model="form.marginAmount"
              :step="1"
              :min="0"
              :max="999999999999"
              :precision="decimalPrecision.contract"
              :controls="false"
              controls-position="right"
              class="input-underline"
              style="width: 220px"
              placeholder="保证金(元)"
            />
          </div>
        </el-form-item>
        <el-form-item label="保证金类型" prop="marginType">
          <common-select
            v-model="form.marginType"
            :options="dict.margin_type"
            type="dict"
            size="small"
            clearable
            placeholder="保证金类型"
            class="input-underline"
            style="width: 220px"
          />
        </el-form-item>
        <el-form-item label="币种" prop="currencyType">
          <common-select
            v-model="form.currencyType"
            :options="dict.currency_type"
            type="dict"
            size="small"
            clearable
            placeholder="币种"
            class="input-underline"
            style="width: 220px"
          />
        </el-form-item>
      </div>
    </div>
    <el-divider><span class="title">合同附件</span></el-divider>
    <div class="upload-box">
      <upload-list
        showView
        show-download
        :file-classify="fileClassifyEnum.CONTRACT_ATT.V"
        v-model:files="form.attachmentFiles"
        empty-text="暂未上传合同附件"
      />
    </div>
  </el-form>
</template>

<script setup>
import { ref, defineProps, watch, computed, defineExpose, nextTick } from 'vue'
import { dateDifference } from '@/utils/date'
import regionCascader from '@comp-base/region-cascader'
import userDeptCascader from '@comp-base/user-dept-cascader.vue'
import useDict from '@compos/store/use-dict'
import { orderSourceTypeEnum } from '@enum-ms/contract'
import { fileClassifyEnum } from '@enum-ms/file'
import uploadList from '@comp/file-upload/UploadList.vue'
import useWatchFormValidate from '@compos/form/use-watch-form-validate'
import { DP } from '@/settings/config'
import { digitUppercase } from '@/utils/data-type/number'
import useDecimalPrecision from '@compos/store/use-decimal-precision'

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
  startDate: String(new Date().getTime()), // 项目开始时间
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
  signerId: undefined // 销售签约人
}

const form = ref(JSON.parse(JSON.stringify(defaultForm)))

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
  startDate: [{ required: true, message: '请选择开工日期', trigger: 'change' }],
  endDate: [{ required: true, message: '请选择完工日期', trigger: 'change' }],
  name: [
    { required: true, message: '请填写项目名称', trigger: 'blur' },
    { min: 1, max: 60, message: '长度在 1 到 60 个字符', trigger: 'blur' }
  ],
  shortName: [
    { required: true, message: '请填写项目简称', trigger: 'blur' },
    { min: 1, max: 12, message: '长度在 1 到 12 个字符', trigger: 'blur' }
  ],
  orderSourceType: [{ required: true, message: '请选择订单来源', trigger: 'change' }],
  contractAmount: [{ required: true, validator: validateMoney, trigger: 'blur' }],
  address: [{ max: 220, message: '长度不超过 220 个字符', trigger: 'blur' }],
  signingAddress: [{ max: 220, message: '长度不超过 220 个字符', trigger: 'blur' }]
}

const props = defineProps({
  formData: {
    type: Object,
    default: () => {}
  }
})

watch(
  () => props.formData,
  (val) => {
    resetForm(val)
  },
  { deep: true, immediate: true }
)

const managementFee = computed(() => {
  if (form.value.managementFeeRate && form.value.contractAmount) {
    return ((form.value.managementFeeRate * form.value.contractAmount) / 100).toFixed(decimalPrecision.contract)
  }
  return undefined
})

/**
 * 重置表单
 */
function resetForm(data) {
  // 清除表单信息
  if (formRef.value) {
    formRef.value.resetFields()
  }
  let formVal
  if (data && Object.keys(data).length > 0) {
    formVal = data
  } else {
    formVal = JSON.parse(JSON.stringify(defaultForm))
  }
  form.value = JSON.parse(JSON.stringify(formVal))
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
    if (valid) {
      const data = JSON.parse(JSON.stringify(form.value))
      data.attachments = data.attachmentFiles.length > 0 ? data.attachmentFiles.map((v) => v.id) : []
      Object.assign(props.formData, data)
    }
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
    return time.getTime() - 8.64e6 < new Date().getTime()
  }
}

function handleRegionChange(val) {
  form.value.countryId = undefined
  form.value.provinceId = undefined
  form.value.cityId = undefined
  form.value.regionId = undefined
  val &&
    val.forEach((v, i) => {
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

defineExpose({
  validateForm
})
</script>
<style lang="scss" scoped>
.upload-box {
  box-sizing: border-box;
  padding: 0 25px;
}
::v-deep(.input-underline) {
  // width: calc((95vw - 40px)/3);
  width: 220px;
  margin-right: 0;
  input {
    border-top: 0;
    border-left: 0;
    border-right: 0;
    border-radius: 0;
  }
}
.form-row {
  width: 100%;
}
.form-margin{
  ::v-deep(.el-form-item){
    margin-right:30px;
  }
}
</style>
