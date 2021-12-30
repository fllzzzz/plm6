<template>
  <common-dialog
    append-to-body
    :close-on-click-modal="false"
    :before-close="crud.cancelCU"
    :visible="crud.status.cu > 0"
    :title="crud.status.title"
    :show-close="true"
    width="80%"
    fullscreen
  >
    <template #titleRight>
      <common-button :loading="crud.status.cu === CRUD.STATUS.PROCESSING" size="mini" type="primary" @click="crud.submitCU">提 交</common-button>
      <store-operation v-if="crud.status.add > CRUD.STATUS.NORMAL" type="crud" />
    </template>
    <div class="form">
      <el-form v-loading="crud.editDetailLoading" :disabled="crud.status.cu === CRUD.STATUS.PROCESSING" ref="formRef" :model="form" :rules="rules" size="small" label-width="100px" class="demo-form">
        <div class="rule-row">
          <el-form-item label="供应商名称" prop="name">
            <el-input  v-model="form.name" maxlength="32" show-word-limit placeholder="请输入供应商名称" />
          </el-form-item>
          <el-form-item label="供应商分类" prop="supplierClass">
            <common-select
              v-model="form.supplierClass"
              :options="supplierClassEnum.ENUM"
              type="enum"
              clearable
              multiple
              placeholder="请选择供应商分类"
              style="width:100%;"
              size="medium"
              @change="handleSupplierClass"
            />
          </el-form-item>
          <el-form-item label="主营业务" prop="mainBusiness">
            <el-input  v-model="form.mainBusiness" placeholder="请输入主营业务" maxlength="32" />
          </el-form-item>
        </div>
        <div class="rule-row">
            <el-form-item label="选择地区" prop="area">
              <region-cascader
                ref="region"
                style="width:100%;"
                v-model="form.area"
                clearable
                filterable
                @change="handleRegionChange"
              />
            </el-form-item>
            <el-form-item label="详细地址" prop="address">
              <el-input v-model="form.address" placeholder="请您输入详细地址" maxlength="250" />
            </el-form-item>
            <el-form-item/>
        </div>
        <div class="rule-row">
          <el-form-item label="社会统一代码" prop="socialCode">
            <el-input  v-model="form.socialCode" placeholder="请输入社会统一代码" maxlength="18" />
          </el-form-item>
          <el-form-item label="成立日期" prop="registrationDate">
            <el-date-picker
              v-model="form.registrationDate"
              type="date"
              style="width:100%;"
              value-format="timestamp"
              placeholder="选择日期"
            />
          </el-form-item>
          <el-form-item label="营业期限" prop="businessTerm">
            <el-input v-model="form.businessTerm" placeholder="请输入营业期限" maxlength="32" />
          </el-form-item>
        </div>
        <div class="rule-row">
          <el-form-item label="法定代表人" prop="legalRepresentative">
            <el-input v-model="form.legalRepresentative" placeholder="请输入法定代表人" maxlength="32" />
          </el-form-item>
          <el-form-item label="注册资本" prop="registeredCapital">
            <el-input v-model="form.registeredCapital" placeholder="请输入注册资本" maxlength="32" />
          </el-form-item>
          <el-form-item label="企业类型" prop="enterpriseType">
            <common-select
              v-model="form.enterpriseType"
              :options="dict.enterprise_type"
              type="dict"
              clearable
              placeholder="请选择企业类型"
              style="width:100%;"
              size="medium"
            />
          </el-form-item>
        </div>
        <div class="rule-row">
          <el-form-item label="开户行名称" prop="bankName">
            <el-input v-model="form.bankName" placeholder="请输入开户行名称"  maxlength="32"/>
          </el-form-item>
          <el-form-item label="银行账户" prop="bankAccount">
            <el-input v-model="form.bankAccount" placeholder="请输入银行账户" maxlength="32"/>
          </el-form-item>
          <el-form-item/>
        </div>
        <div class="rule-row">
          <el-form-item label="公司官网" prop="website">
            <el-input v-model="form.website" placeholder="请输入公司官网网址" maxlength="64" />
          </el-form-item>
          <el-form-item label="公司邮箱" prop="companyEmail">
            <el-input v-model="form.companyEmail" placeholder="请输入公司邮箱" maxlength="64" />
          </el-form-item>
          <el-form-item label="公司电话" prop="companyPhone">
            <el-input v-model="form.companyPhone" placeholder="请输入公司电话" maxlength="32" />
          </el-form-item>
        </div>
        <div v-for="(item,index) in form.contacts" :key="index" class="rule-row">
          <el-form-item label="联系人">
            <el-input v-model="item.name" placeholder="请输入联系人"  maxlength="32" />
          </el-form-item>
          <el-form-item label="联系电话" :prop="`contacts[${index}].phone`" :rules="contactsRules.phone">
            <el-input v-model="item.phone" placeholder="请输入联系电话"  maxlength="32" />
          </el-form-item>
          <el-form-item label="个人邮箱" :prop="`contacts[${index}].email`" :rules="contactsRules.email">
            <el-input v-model="item.email" placeholder="请输入个人邮箱"  maxlength="64" />
          </el-form-item>
          <div style="line-height: 32px;margin-left:-26px;">
            <common-button v-if="index === form.contacts.length-1" type="primary" size="mini" circle icon="el-icon-plus" @click="form.contacts.push({...contact})" />
            <common-button v-else type="danger" size="mini" circle icon="el-icon-minus" @click="form.contacts.splice(index, 1)" />
          </div>
        </div>
        <div class="item-center">
          <upload-list
            :show-download="!!form.id"
            :file-classify="fileClassifyEnum.SUPPLIER_ATT.V"
            :download-perm="crud.permission.downloadAttachments"
            :download-fn="downloadAttachment"
            v-model:files="form.files"
            style="padding: 10px 30px 0"
          />
        </div>
      </el-form>
    </div>
  </common-dialog>
</template>

<script setup>
import { downloadAttachment } from '@/api/wms/supplier/manage'
import { ref } from 'vue'

import { supplierClassEnum } from '@enum-ms/supplier'
import { fileClassifyEnum } from '@enum-ms/file'
import { getBitwiseBack } from '@data-type/number'
import { validatorTel, validatorWebsite, validatorEmail } from '@/utils/validate/pattern'

import { regForm } from '@compos/use-crud'
import useDict from '@compos/store/use-dict'
import storeOperation from '@crud/STORE.operation'
import regionCascader from '@comp-base/region-cascader'
import uploadList from '@comp/file-upload/UploadList.vue'

const formRef = ref()
const contact = ref({ name: '', phone: '', email: '' })

const defaultForm = {
  name: '',
  area: [],
  address: '',
  socialCode: '',
  registrationDate: '',
  businessTerm: '',
  legalRepresentative: '',
  registeredCapital: '',
  enterpriseType: '',
  enterpriseTypeName: '',
  bankName: '',
  contacts: [{ ...contact.value }],
  bankAccount: '',
  website: '',
  companyEmail: '',
  companyPhone: '',
  mainBusiness: '',
  files: [],
  attachments: [],
  supplierClass: [],
  supplierClassification: undefined
}

const dict = useDict(['enterprise_type'])
const { crud, form, CRUD } = regForm(defaultForm, formRef)

const rules = {
  name: [
    { required: true, message: '请输入供应商名称', trigger: 'blur' },
    { min: 2, max: 32, message: '长度在 2 到 32 个字符', trigger: 'blur' }
  ],
  supplierClass: [{ required: true, message: '请选择供应商分类', trigger: 'change' }],
  website: [{ pattern: validatorWebsite, message: '请填写正确的公司官网网址', trigger: 'blur' }],
  companyEmail: [{ pattern: validatorEmail, message: '请填写正确的公司邮箱', trigger: 'blur' }],
  companyPhone: [{ pattern: validatorTel, message: '请填写正确的公司电话【手机号/固话】', trigger: 'blur' }]
}

const contactsRules = {
  email: [{ pattern: validatorEmail, message: '请填写正确的个人邮箱', trigger: 'blur' }],
  phone: [{ pattern: validatorTel, message: '请填写正确的联系电话【手机号/固话】', trigger: 'blur' }]
}

// 编辑时详情加载完成后
CRUD.HOOK.beforeEditDetailLoaded = async (crud, form) => {
  try {
    const supplierDetail = { ...form }
    const list = [supplierDetail.countryId, supplierDetail.provinceId, supplierDetail.cityId, supplierDetail.regionId]
    supplierDetail.area = list.filter(val => {
      return !(!val || val === '')
    })
    supplierDetail.files = supplierDetail.attachments || []
    supplierDetail.supplierClass = getBitwiseBack(supplierDetail.supplierClassification)
    Object.assign(crud.form, supplierDetail)
  } catch (error) {
    crud.notify('获取供应商详情失败', CRUD.NOTIFICATION_TYPE.ERROR)
  }
}

// 提交前
CRUD.HOOK.beforeSubmit = async () => {
  crud.form.attachments = crud.form.files.map(f => f.id)
  crud.form.contacts = crud.form.contacts.filter(v => v.name || v.phone || v.email)
}

// 地区选择
function handleRegionChange(val = []) {
  const keys = ['countryId', 'provinceId', 'cityId', 'regionId']
  keys.forEach((key, index) => {
    crud.form[key] = val && val[index] || undefined
  })
}

// 供应商分类选择
function handleSupplierClass(val) {
  let supplierClass
  if (val) {
    val.forEach(v => {
      supplierClass |= v
    })
  }
  crud.form.supplierClassification = supplierClass
}
</script>

<style rel="stylesheet/scss" lang="scss" scoped>
.form {
  padding:  0px 60px 35px 35px;
}
.demo-form .rule-row {
  display: flex;
  margin-bottom: 10px;
}
.demo-form .rule-row:last-child {
  margin-bottom: 0px;
}
.form .el-form-item {
  flex: 1;
  width: 33%;
  margin-right: 30px;
}
.form .el-upload__tip {
  padding-left: 15px;
}
</style>
