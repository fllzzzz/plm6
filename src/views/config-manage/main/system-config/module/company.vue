<template>
  <el-card v-if="checkPermission(permission.get)" class="box-card" shadow="hover">
    <template #header class="clearfix">
      <span class="title">公司信息配置</span>
      <div v-if="checkPermission(permission.edit) && changed" style="float: right">
        <common-button size="small" type="warning" @click="cancel">取消修改</common-button>
        <common-button :loading="saveLoading" size="small" type="success" @click="submit">保存</common-button>
      </div>
    </template>
    <div v-loading="loading || saveLoading">
      <el-form ref="companyFormRef" :model="form" :rules="rules" class="demo-ruleForm" label-width="80px">
        <el-form-item label="公司编号" prop="companyNo">
          <div class="input-underline">
            <el-input
              v-if="checkPermission(permission.edit)"
              v-model="form.companyNo"
              type="text"
              placeholder="请填写公司编号"
              style="width: 500px;"
              autocomplete="off"
            />
            <span v-else v-empty-text="form.companyNo"/>
          </div>
        </el-form-item>
        <el-form-item label="公司名称" prop="companyName">
          <div class="input-underline">
            <el-input
              v-if="checkPermission(permission.edit)"
              v-model="form.companyName"
              type="text"
              placeholder="请填写公司名称"
              autocomplete="off"
              style="width: 500px;"
            />
            <span v-else v-empty-text="form.companyName"/>
          </div>
        </el-form-item>
        <el-form-item label="公司网址" prop="website">
          <div class="input-underline">
            <el-input
              v-if="checkPermission(permission.edit)"
              v-model="form.website"
              type="text"
              placeholder="请填写公司网址"
              maxlength="32"
              autocomplete="off"
              style="width: 500px;"
            />
            <span v-else v-empty-text="form.website"/>
          </div>
        </el-form-item>
        <el-form-item label="公司电话" prop="telephone">
          <div class="input-underline">
            <el-input
              v-if="checkPermission(permission.edit)"
              v-model="form.telephone"
              type="text"
              placeholder="请填写公司电话"
              maxlength="32"
              autocomplete="off"
              style="width: 500px;"
            />
            <span v-else v-empty-text="form.telephone"/>
          </div>
        </el-form-item>
      </el-form>
    </div>
  </el-card>
</template>

<script setup>
import { getCompanyConfig, setCompanyConfig } from '@/api/config/main/system-config'
import { ref, computed, nextTick } from 'vue'
import { useStore } from 'vuex'

import checkPermission from '@/utils/system/check-permission'
import { isObjectValueEqual } from '@data-type/object'
import { validatorEnOrNum, validatorCN } from '@/utils/validate/pattern'

import { systemConfigPM } from '@/page-permission/config'
import { ElMessage } from 'element-plus'

const permission = systemConfigPM.company
const defaultData = {
  companyNo: void 0,
  companyName: void 0,
  website: void 0,
  telephone: void 0
}

const store = useStore()
const companyFormRef = ref()
const loading = ref(false)
const saveLoading = ref(false)
const form = ref({ ...defaultData })
const sourceData = ref({ ...defaultData })
const rules = {
  companyNo: [
    { required: true, message: '请填写公司编号', trigger: 'blur' },
    { min: 2, max: 10, message: '编号长度为2-10个字符', trigger: 'blur' },
    { pattern: validatorEnOrNum.pattern, message: validatorEnOrNum.message, trigger: 'blur' }
  ],
  companyName: [
    { required: true, message: '请填写公司名称', trigger: 'blur' },
    { min: 2, max: 60, message: '名称长度不少于2个字，不多于60个字', trigger: 'blur' },
    { pattern: validatorCN.pattern, message: validatorCN.message, trigger: 'blur' }
  ]
}

const changed = computed(() => {
  return !isObjectValueEqual(form.value, sourceData.value)
})

fetch()

async function fetch() {
  if (!checkPermission(permission.get)) return
  loading.value = true
  try {
    const { companyNo, companyName, website, telephone } = await getCompanyConfig() || {}
    sourceData.value = { companyNo, companyName, website, telephone }
    form.value = { companyNo, companyName, website, telephone }
  } catch (error) {
    console.log('error', error)
  } finally {
    loading.value = false
  }
}

function submit() {
  companyFormRef.value.validate(async (valid) => {
    if (valid) {
      saveLoading.value = true
      try {
        await setCompanyConfig(form.value)
        sourceData.value = { ...form.value }
        ElMessage.success('修改成功')
        store.dispatch('config/fetchCompany')
      } catch (error) {
        console.log('设置基础信息：项目信息', error)
      } finally {
        saveLoading.value = false
      }
    } else {
      return false
    }
  })
}

function cancel() {
  form.value = { ...sourceData.value }
  nextTick(() => {
    companyFormRef.value.clearValidate()
  })
}

</script>

<style lang="scss" scoped>
.title{
    height: 32px;
    line-height: 32px;
}
</style>
