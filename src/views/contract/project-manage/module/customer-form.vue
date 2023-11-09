<template>
  <el-form ref="formRef" :model="form" :rules="rules" inline size="small" label-width="110px" class="form-margin">
    <div>
      <div class="form-row">
        <el-form-item label="客户名称" prop="customerUnit">
          <el-input v-model.trim="form.customerUnit" class="input-underline" placeholder="客户名称" maxlength="30" style="width:320px;"/>
        </el-form-item>
        <el-form-item label="社会统一代码" prop="socialCode">
          <el-input v-model.trim="form.socialCode" class="input-underline" placeholder="社会统一代码" maxlength="18" style="width:320px;"/>
        </el-form-item>
      </div>
      <div class="form-row">
        <el-form-item label="联系电话" prop="customerUnitPhone">
          <el-input v-model.trim="form.customerUnitPhone" class="input-underline" placeholder="联系电话" style="width:320px;"/>
        </el-form-item>
        <el-form-item label="邮箱" prop="customerEmail">
          <el-input v-model.trim="form.customerEmail" class="input-underline" placeholder="邮箱" maxlength="256" style="width:320px;"/>
        </el-form-item>
      </div>
      <div class="form-row">
        <el-form-item label="省市区" prop="region">
          <region-cascader
            class="input-underline"
            ref="region"
            v-model="form.region"
            clearable
            filterable
            @change="handleRegionChange"
            style="width:320px;"
          />
        </el-form-item>
        <el-form-item label="详细地址" prop="customerAddress">
          <el-input v-model.trim="form.customerAddress" placeholder="详细地址" class="input-underline" maxlength="200" style="width:380px;"/>
        </el-form-item>
      </div>
      <!--      <el-divider><span class="title">收款信息</span></el-divider>-->
      <div class="form-row">
        <el-form-item label="银行账户户名" prop="customerBankUserName">
          <el-input
            v-model.trim="form.customerBankUserName"
            placeholder="银行账户户名"
            :controls="false"
            maxlength="50"
            class="input-underline"
            style="width:320px;"
          />
        </el-form-item>
        <el-form-item label="银行账户账号" prop="customerBankCode">
          <el-input v-model.trim="form.customerBankCode" placeholder="银行账号" :controls="false" maxlength="30" class="input-underline" />
        </el-form-item>
        <el-form-item label="开户行" prop="customerBankName">
          <el-input v-model.trim="form.customerBankName" placeholder="开户行" class="input-underline" maxlength="30"/>
        </el-form-item>
      </div>
      <!--      <el-divider><span class="title">负责人</span></el-divider>-->
      <div class="form-row">
        <el-form-item label="收货负责人" prop="receivingManager">
          <el-input v-model.trim="form.receivingManager" placeholder="收货负责人" class="input-underline" style="width:320px;" maxlength="20" />
        </el-form-item>
        <el-form-item label="联系电话" prop="receivingManagerPhone">
          <el-input v-model.trim="form.receivingManagerPhone" placeholder="联系电话" class="input-underline" />
        </el-form-item>
      </div>
      <div class="form-row">
        <el-form-item label="结算负责人" prop="settleManager">
          <el-input v-model.trim="form.settleManager" placeholder="结算负责人" class="input-underline" style="width:320px;" maxlength="20" />
        </el-form-item>
        <el-form-item label="联系电话" prop="settleManagerPhone">
          <el-input v-model.trim="form.settleManagerPhone" placeholder="联系电话" class="input-underline" />
        </el-form-item>
      </div>
      <div class="form-row">
        <el-form-item label="财务负责人" prop="financeManager">
          <el-input v-model.trim="form.financeManager" placeholder="财务负责人" class="input-underline" style="width:320px;" maxlength="20" />
        </el-form-item>
        <el-form-item label="联系电话" prop="financeManagerPhone">
          <el-input v-model.trim="form.financeManagerPhone" placeholder="联系电话" class="input-underline" />
        </el-form-item>
      </div>
      <div class="form-row">
        <el-form-item label="审核负责人" prop="auditManager">
          <el-input v-model.trim="form.auditManager" placeholder="审核负责人" class="input-underline" style="width:320px;" maxlength="20" />
        </el-form-item>
        <el-form-item label="联系电话" prop="auditManagerPhone">
          <el-input v-model.trim="form.auditManagerPhone" placeholder="联系电话" class="input-underline" />
        </el-form-item>
      </div>
      <div class="form-row">
        <el-form-item label="设计负责人" prop="designManager">
          <el-input v-model.trim="form.designManager" placeholder="设计负责人" class="input-underline" style="width:320px;" maxlength="20" />
        </el-form-item>
        <el-form-item label="联系电话" prop="designManagerPhone">
          <el-input v-model.trim="form.designManagerPhone" placeholder="联系电话" class="input-underline" />
        </el-form-item>
      </div>
      <div>
          <el-form-item label="备注" prop="customerRemark">
            <el-input  v-model.trim="form.customerRemark" type="textarea" maxlength="1000" size="large" />
          </el-form-item>
        </div>
    </div>
  </el-form>
</template>

<script setup>
import { ref, defineProps, watch, defineExpose, nextTick } from 'vue'
import regionCascader from '@comp-base/region-cascader'
import useWatchFormValidate from '@compos/form/use-watch-form-validate'
import { validatorTel, validatorEnOrNum, validatorNatural } from '@/utils/validate/pattern'

const formRef = ref()
const defaultForm = {
  customerUnit: undefined, // 客户名称
  socialCode: undefined, // 社会统一代码
  customerEmail: undefined, // 客户邮箱
  customerUnitPhone: undefined, // 客户联系电话
  region: [], // 项目省市区
  customerCountryId: undefined, // 国家
  customerProvinceId: undefined, // 省
  customerCityId: undefined, // 市
  customerRegionId: undefined, // 区
  customerAddress: undefined, // 地址
  customerBankName: undefined, // 开户行
  customerBankCode: undefined, // 银行账号
  receivingManager: undefined,
  receivingManagerPhone: undefined,
  designManager: undefined,
  designManagerPhone: undefined,
  financeManager: undefined,
  financeManagerPhone: undefined,
  settleManager: undefined,
  settleManagerPhone: undefined,
  auditManager: undefined,
  auditManagerPhone: undefined,
  customerRemark: undefined
}

const form = ref(JSON.parse(JSON.stringify(defaultForm)))
const rules = {
  customerUnit: [{ required: true, max: 30, message: '必填,长度不超过 30 个字符', trigger: 'blur' }],
  socialCode: [
    { max: 18, message: '长度不超过 18 个字符', trigger: 'blur' },
    { pattern: validatorEnOrNum.pattern, message: validatorEnOrNum.message }
  ],
  customerEmail: [{ type: 'email', message: '请填写正确的邮箱地址', trigger: 'blur' }],
  customerUnitPhone: [{ pattern: validatorTel, message: '请填写正确的联系电话【手机号/固话】', trigger: 'blur' }],
  customerAddress: [{ max: 200, message: '长度不超过 200 个字符', trigger: 'blur' }],
  customerBankName: [{ max: 50, message: '长度不超过 50 个字符', trigger: 'blur' }],
  customerBankCode: [
    { max: 30, message: '长度不超过 30 个字符', trigger: 'blur' },
    { pattern: validatorNatural, message: '请输入数字', trigger: 'blur' }
  ],
  receivingManager: [{ required: true, max: 20, message: '长度不超过 20个字符', trigger: 'blur' }],
  receivingManagerPhone: [{ required: true, pattern: validatorTel, message: '请填写正确的联系电话【手机号/固话】', trigger: 'blur' }],
  designManagerPhone: [{ pattern: validatorTel, message: '请填写正确的联系电话【手机号/固话】', trigger: 'blur' }],
  financeManagerPhone: [{ pattern: validatorTel, message: '请填写正确的联系电话【手机号/固话】', trigger: 'blur' }],
  settleManagerPhone: [{ pattern: validatorTel, message: '请填写正确的联系电话【手机号/固话】', trigger: 'blur' }],
  auditManagerPhone: [{ pattern: validatorTel, message: '请填写正确的联系电话【手机号/固话】', trigger: 'blur' }]
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

function handleRegionChange(val) {
  form.value.customerCountryId = undefined
  form.value.customerProvinceId = undefined
  form.value.customerCityId = undefined
  form.value.customerRegionId = undefined
  val &&
    val.forEach((v, i) => {
      if (i === 0) {
        form.value.customerCountryId = v
      }
      if (i === 1) {
        form.value.customerProvinceId = v
      }
      if (i === 2) {
        form.value.customerCityId = v
      }
      if (i === 3) {
        form.value.customerRegionId = v
      }
    })
}

async function validateForm() {
  try {
    const valid = await formRef.value.validate()
    if (valid) {
      Object.assign(props.formData, JSON.parse(JSON.stringify(form.value)))
    }
    return valid
  } catch (error) {
    console.log('error', error)
    return false
  }
}

function resetForm(data) {
  if (formRef.value) {
    formRef.value.resetFields()
  }
  let formVal
  if (data && Object.keys(data).length > 0) {
    formVal = JSON.parse(JSON.stringify(data))
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

defineExpose({
  validateForm
})
</script>
<style lang="scss" scoped>
::v-deep(.el-textarea__inner) {
  width: 700px;
  height: 150px;
}
::v-deep(.input-underline) {
  // width: calc((95vw - 40px)/3);
  width: 250px;
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
