<template>
  <el-card shadow="always">
    <template #header>
      <div class="clearfix">
        <span class="card-title">过磅短信配置</span>
        <common-tip-button
          v-permission="permission.overweightSMSRecipientEdit"
          :loading="submitLoading"
          :disabled="submitDisabled"
          show-tip
          size="mini"
          type="primary"
          style="float: right"
          @click="submit"
        >
          保存
        </common-tip-button>
      </div>
    </template>
    <el-form ref="formRef" v-loading="dataLoading" :model="form" :rules="rules" :disabled="formDisabled" label-width="100px">
      <el-form-item label="短信接收人" prop="id">
        <user-select
          v-if="checkPermission(permission.overweightSMSRecipientEdit)"
          v-model="form.id"
          ref="userSelectRef"
          size="small"
          placeholder="请选择过磅短信接收人"
          style="width: 200px"
        />
        <span v-else>{{ form.name }}</span>
      </el-form-item>
      <el-form-item label="手机号" prop="phone">
        <el-input v-if="checkPermission(permission.overweightSMSRecipientEdit)" v-model="form.phone" size="small" style="width: 200px" />
        <span v-else>{{ form.phone }}</span>
      </el-form-item>
      <el-form-item label="过磅超标重量允许值" prop="maxWeight" label-width="150px">
        <el-input-number
          v-if="checkPermission(permission.overweightSMSRecipientEdit)"
          v-model.number="form.maxWeight"
          :min="0"
          :max="100"
          :step="1"
          :precision="2"
          placeholder="比例"
          :controls="false"
          style="width: 80px"
          class="input-underline"
        />
        <span v-else>{{ form.maxWeight }}</span>
        %
      </el-form-item>
    </el-form>
  </el-card>
</template>

<script setup>
import { getOverweightSMSRecipient as getConfig, setOverweightSMSRecipient as setConfig } from '@/api/config/mes/base'
import { ref, computed, onMounted, inject, watch } from 'vue'

import { isObjectValueEqual } from '@data-type/object'
import { deepClone } from '@/utils/data-type'
import { isNotBlank } from '@data-type/index'
import { validatorPhone } from '@/utils/validate/pattern'
import checkPermission from '@/utils/system/check-permission'

import userSelect from '@comp-common/user-select'
import { ElNotification } from 'element-plus'

const permission = inject('permission')

const rules = {
  id: [{ required: true, message: '请选择过磅短信接收人', trigger: 'change' }],
  phone: [
    { required: true, message: '请填写手机号', trigger: 'blur' },
    { pattern: validatorPhone, message: '请填写正确的手机号', trigger: 'blur' }
  ]
}
const formRef = ref()
const userSelectRef = ref()
// 数据源
const dataSource = ref({
  id: undefined,
  name: undefined,
  phone: undefined,
  maxWeight: undefined
})
// 表单
const form = ref(dataSource.value)
// loading
const dataLoading = ref(false)
const submitLoading = ref(false)
const init = ref(false)

const submitDisabled = computed(() => isObjectValueEqual(form.value, dataSource.value))
const formDisabled = computed(() => dataLoading.value || submitLoading.value)

onMounted(() => {
  fetchData()
})

watch(
  () => form.value.id,
  (val, oldVal) => {
    if (val && !init.value) {
      const { phone } = userSelectRef.value.getUser(val)
      form.value.phone = phone
    }
    init.value = false
  }
)

async function fetchData() {
  dataLoading.value = true
  try {
    const { id, name, phone, maxWeight } = await getConfig()
    form.value = { id, name, phone, maxWeight: isNotBlank(maxWeight) ? maxWeight : undefined }
    dataSource.value = { id, name, phone, maxWeight: isNotBlank(maxWeight) ? maxWeight : undefined }
    init.value = true
  } catch (error) {
    console.log('获取过磅超标短信接收人', error)
  } finally {
    dataLoading.value = false
  }
}

async function submit() {
  try {
    const passed = await formRef.value.validate()
    if (!passed) return
    submitLoading.value = true
    await setConfig(form.value)
    ElNotification({
      title: '过磅超标短信接收人配置成功',
      type: 'success',
      duration: 2500
    })
    dataSource.value = deepClone(form.value)
  } catch (error) {
    console.log('设置过磅超标短信接收人', error)
  } finally {
    submitLoading.value = false
  }
}
</script>
