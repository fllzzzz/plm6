<template>
  <div id="pageContainer" class="app-container">
    <el-form ref="formRef" :model="form" :rules="rules" inline size="small" label-position="right" label-width="110px">
      <div>
        <div class="form-row">
          <el-form-item label="客户名称" prop="customerUnit">
            <div class="input-underline">
              <el-input v-if="isModify" v-model="form.customerUnit" placeholder="客户名称" maxlength="20"/>
              <span v-else>{{ detail.customerUnit }}</span>
            </div>
          </el-form-item>
          <el-form-item label="社会统一代码" prop="socialCode">
            <div class="input-underline">
              <el-input v-if="isModify" v-model="form.socialCode" placeholder="社会统一代码" maxlength="18"/>
              <span v-else>{{ detail.socialCode }}</span>
            </div>
          </el-form-item>
          <el-form-item label="联系电话" prop="customerUnitPhone">
            <div class="input-underline">
              <el-input v-if="isModify" v-model="form.customerUnitPhone" placeholder="联系电话" />
              <span v-else>{{ detail.customerUnitPhone }}</span>
            </div>
          </el-form-item>
        </div>
        <div class="form-row">
          <el-form-item label="省市区" prop="region">
            <div class="input-underline">
              <region-cascader
                v-if="isModify"
                class="input-underline"
                ref="region"
                style="width: 200px"
                v-model="form.region"
                clearable
                filterable
                @change="handleRegionChange"
              />
              <template v-else><span>{{detail.customerCountryName}}</span><span>{{detail.customerProvinceName}}</span><span>{{detail.customerCityName}}</span><span>{{detail.customerRegionName}}</span></template>
            </div>
          </el-form-item>
          <el-form-item label="邮箱" prop="customerEmail">
            <div class="input-underline">
              <el-input v-if="isModify" v-model="form.customerEmail" placeholder="邮箱" maxlength="256"/>
              <span class="detail-break" v-else>{{ detail.customerEmail }}</span>
            </div>
          </el-form-item>
          <el-form-item label="详细地址" prop="customerAddress">
            <div class="input-underline">
              <el-input v-if="isModify" v-model="form.customerAddress" placeholder="详细地址" maxlength="200"/>
              <span class="detail-break" v-else>{{ detail.customerAddress }}</span>
            </div>
          </el-form-item>
        </div>
        <div class="form-row">
          <el-form-item label="银行账户户名" prop="customerBankUserName">
            <div class="input-underline">
              <el-input
                v-if="isModify"
                v-model="form.customerBankUserName"
                placeholder="银行账户户名"
                :controls="false"
                maxlength="50"
                class="input-underline"
              />
              <span class="detail-break" v-else>{{ detail.customerBankUserName }}</span>
            </div>
          </el-form-item>
          <el-form-item label="银行账号" prop="customerBankCode">
            <div class="input-underline">
              <el-input v-if="isModify" v-model="form.customerBankCode" placeholder="银行账号" :controls="false" maxlength="30" />
              <span v-else>{{ detail.customerBankCode }}</span>
            </div>
          </el-form-item>
          <el-form-item label="开户行" prop="customerBankName">
            <div class="input-underline">
              <el-input v-if="isModify" v-model="form.customerBankName" placeholder="开户行" maxlength="30"/>
              <span v-else>{{ detail.customerBankName }}</span>
            </div>
          </el-form-item>
        </div>
        <div class="form-row">
          <el-form-item label="负责人1" prop="customerManagerOne">
            <div class="input-underline">
              <el-input v-if="isModify" v-model="form.customerManagerOne" placeholder="负责人1" maxlength="20"/>
              <span v-else>{{ detail.customerManagerOne }}</span>
            </div>
          </el-form-item>
          <el-form-item label="联系电话" prop="customerManagerOnePhone">
            <div class="input-underline">
              <el-input v-if="isModify" v-model.number="form.customerManagerOnePhone" placeholder="负责人1联系电话" />
              <span v-else>{{ detail.customerManagerOnePhone }}</span>
            </div>
          </el-form-item>
        </div>
        <div class="form-row">
          <el-form-item label="负责人2" prop="customerManagerTwo">
            <div class="input-underline">
              <el-input v-if="isModify" v-model="form.customerManagerTwo" placeholder="负责人2" maxlength="20"/>
              <span v-else>{{ detail.customerManagerTwo }}</span>
            </div>
          </el-form-item>
          <el-form-item label="联系电话" prop="customerManagerTwoPhone">
            <div class="input-underline">
              <el-input v-if="isModify" v-model.number="form.customerManagerTwoPhone" placeholder="负责人2联系电话" />
              <span v-else>{{ detail.customerManagerTwoPhone }}</span>
            </div>
          </el-form-item>
        </div>
      </div>
    </el-form>
  </div>
</template>

<script setup>
import { ref, defineProps, watch, defineExpose } from 'vue'
import regionCascader from '@comp-base/region-cascader'
import useWatchFormValidate from '@compos/form/use-watch-form-validate'
import { validatorTel, validatorEnOrNum, validatorNatural } from '@/utils/validate/pattern'
import { getContractCustomer } from '@/api/contract/project'
import { cleanArray } from '@data-type/array'

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
  customerManagerOne: undefined, // 负责人1名称
  customerManagerOnePhone: undefined, // 负责人1联系电话
  customerManagerTwo: undefined, // 负责人2名称
  customerManagerTwoPhone: undefined // 负责人2联系电话
}

const form = ref(JSON.parse(JSON.stringify(defaultForm)))
const detail = ref(JSON.parse(JSON.stringify(defaultForm)))
const rules = {
  customerUnit: [{ max: 50, message: '长度不超过 50 个字符', trigger: 'blur' }],
  socialCode: [
    { max: 50, message: '长度不超过 30 个字符', trigger: 'blur' },
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
  customerManagerOne: [{ max: 20, message: '长度不超过 20个字符', trigger: 'blur' }],
  customerManagerOnePhone: [{ pattern: validatorTel, message: '请填写正确的联系电话【手机号/固话】', trigger: 'blur' }],
  customerManagerTwo: [{ max: 20, message: '长度不超过 20 个字符', trigger: 'blur' }],
  customerManagerTwoPhone: [{ pattern: validatorTel, message: '请填写正确的联系电话【手机号/固话】', trigger: 'blur' }]
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
    return valid
  } catch (error) {
    console.log('error', error)
    return false
  }
}

function resetForm() {
  if (formRef.value) {
    formRef.value.resetFields()
  }
  form.value = JSON.parse(JSON.stringify(detail.value))
  useWatchFormValidate(formRef, form)
}

async function fetchDetail() {
  if (!props.projectId) {
    return
  }
  let _detail = {}
  try {
    const res = await getContractCustomer(props.projectId)
    console.log(res)
    _detail = JSON.parse(JSON.stringify(res))
    _detail.region = cleanArray([_detail.customerCountryId, _detail.customerProvinceId, _detail.customerCityId, _detail.customerRegionId])
  } catch (error) {
    console.log('error', error)
  } finally {
    detail.value = _detail
    resetForm(detail.value)
  }
}

defineExpose({
  form,
  validateForm,
  fetchDetail,
  resetForm
})
</script>
<style lang="scss" scoped>
.app-container {
  position: relative;
  .operate-btn {
    position: absolute;
    right: 50px;
    top: 20px;
  }
}
::v-deep(.el-input-number .el-input__inner) {
  text-align: left;
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
span {
  // color:#4482ff #1682e6
  color: #82848a;
}
.detail-break{
  word-break:break-all;
}
</style>
