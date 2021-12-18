<template>
  <el-form ref="customerRef" :model="form" :rules="rules" inline size="small" label-position="right" label-width="110px">
    <div>
      <div class="form-row">
        <el-form-item label="客户名称" prop="customerUnit">
          <el-input v-model="form.customerUnit" class="input-underline" placeholder="客户名称" />
        </el-form-item>
        <el-form-item label="社会统一代码" prop="socialCode">
          <el-input v-model="form.socialCode" class="input-underline" placeholder="社会统一代码" />
        </el-form-item>
        <el-form-item label="联系电话" prop="customerUnitPhone">
          <el-input v-model="form.customerUnitPhone" class="input-underline" placeholder="联系电话" />
        </el-form-item>
      </div>
      <div class="form-row">
        <el-form-item label="省市区" prop="region">
          <region-cascader
            class="input-underline"
            ref="region"
            style="width: 200px"
            v-model="form.region"
            clearable
            filterable
            @change="handleRegionChange"
          />
        </el-form-item>
        <el-form-item label="详细地址" prop="customerAddress">
          <el-input v-model="form.customerAddress" placeholder="详细地址" class="input-underline" />
        </el-form-item>
        <el-form-item label="邮箱" prop="customerEmail">
          <el-input v-model="form.customerEmail" class="input-underline" placeholder="邮箱" />
        </el-form-item>
      </div>
      <!--      <el-divider><span class="title">收款信息</span></el-divider>-->
      <div class="form-row">
        <el-form-item label="银行账户户名" prop="customerBankUserName">
          <el-input
            v-model="form.customerBankUserName"
            placeholder="银行账户户名"
            :controls="false"
            maxlength="30"
            class="input-underline"
          />
        </el-form-item>
        <el-form-item label="银行账户账号" prop="customerBankCode">
          <el-input v-model="form.customerBankCode" placeholder="银行账号" :controls="false" maxlength="30" class="input-underline" />
        </el-form-item>
        <el-form-item label="开户行" prop="customerBankName">
          <el-input v-model="form.customerBankName" placeholder="开户行" class="input-underline" />
        </el-form-item>
      </div>
      <!--      <el-divider><span class="title">负责人</span></el-divider>-->
      <div class="form-row">
        <el-form-item label="负责人1" prop="customerManagerOne">
          <el-input v-model="form.customerManagerOne" placeholder="负责人1" class="input-underline" />
        </el-form-item>
        <el-form-item label="联系电话" prop="customerManagerOnePhone">
          <el-input v-model.number="form.customerManagerOnePhone" placeholder="负责人1联系电话" class="input-underline" />
        </el-form-item>
      </div>
      <div class="form-row">
        <el-form-item label="负责人2" prop="customerManagerTwo">
          <el-input v-model="form.customerManagerTwo" placeholder="负责人2" class="input-underline" />
        </el-form-item>
        <el-form-item label="联系电话" prop="customerManagerTwoPhone">
          <el-input v-model.number="form.customerManagerTwoPhone" placeholder="负责人2联系电话" class="input-underline" />
        </el-form-item>
      </div>
    </div>
  </el-form>
</template>

<script setup>
import { ref, defineProps, watch, computed } from 'vue'
import regionCascader from '@comp-base/region-cascader'
import useWatchFormValidate from '@compos/form/use-watch-form-validate'
import { validatorTel, validatorEnOrNum, validatorNatural } from '@/utils/validate/pattern'

const customerRef = ref()
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
  customerManagerOne: undefined, // 负责人1名称
  customerManagerOnePhone: undefined, // 负责人1联系电话
  customerManagerTwo: undefined, // 负责人2名称
  customerManagerTwoPhone: undefined, // 负责人2联系电话
}

const form = ref(JSON.parse(JSON.stringify(defaultForm)))
const rules = {
  customerUnit: [{ max: 50, message: '长度不超过 50 个字符', trigger: 'blur' }],
  socialCode: [
    { max: 50, message: '长度不超过 30 个字符', trigger: 'blur' },
    { pattern: validatorEnOrNum.pattern, message: validatorEnOrNum.message },
  ],
  customerEmail: [{ type: 'email', message: '请填写正确的邮箱地址', trigger: 'blur' }],
  customerUnitPhone: [{ pattern: validatorTel, message: '请填写正确的联系电话【手机号/固话】', trigger: 'blur' }],
  customerAddress: [{ max: 200, message: '长度不超过 200 个字符', trigger: 'blur' }],
  customerBankName: [{ max: 50, message: '长度不超过 50 个字符', trigger: 'blur' }],
  customerBankCode: [
    { max: 30, message: '长度不超过 30 个字符', trigger: 'blur' },
    { pattern: validatorNatural, message: '请输入数字', trigger: 'blur' },
  ],
  customerManagerOne: [{ max: 20, message: '长度不超过 20个字符', trigger: 'blur' }],
  customerManagerOnePhone: [{ pattern: validatorTel, message: '请填写正确的联系电话【手机号/固话】', trigger: 'blur' }],
  customerManagerTwo: [{ max: 20, message: '长度不超过 20 个字符', trigger: 'blur' }],
  customerManagerTwoPhone: [{ pattern: validatorTel, message: '请填写正确的联系电话【手机号/固话】', trigger: 'blur' }],
}

const props = defineProps({
  formData: {
    type: Object,
    default: () => {},
  },
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
    const valid = await customerRef.value.validate()
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
  if (customerRef.value) {
    customerRef.value.resetFields()
  }
  let formKey
  if (data && Object.keys(data).length > 0) {
    formKey = data
  } else {
    formKey = JSON.parse(JSON.stringify(defaultForm))
  }
  const crudFrom = form.value
  for (const key in crudFrom) {
    crudFrom[key] = undefined
  }
  for (const key in form) {
    crudFrom[key] = formKey[key]
  }
  useWatchFormValidate(customerRef, form.value)
}

defineExpose({
  validateForm,
})
</script>
<style lang="scss" scoped>
>>> .input-underline {
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
</style>
