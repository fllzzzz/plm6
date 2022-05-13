<template>
  <common-dialog
    append-to-body
    v-model="visible"
    width="520px"
    title="修改密码"
    :show-close="false"
    :close-on-click-modal="false"
  >
    <el-form ref="formRef" :model="form" :rules="rules" size="small" label-width="88px">
      <el-form-item label="旧密码" prop="oldPwd">
        <el-input v-model="form.oldPwd" :type="passwordType" maxlength="20" auto-complete="on" style="width: 360px;" />
        <span class="show-pwd" @click="showPwd">
          <svg-icon :icon-class="passwordType === 'password' ? 'eye' : 'eye-open'" />
        </span>
      </el-form-item>
      <el-form-item label="新密码" prop="newPwd">
        <el-input v-model="form.newPwd" :type="passwordType" maxlength="20" auto-complete="on" style="width: 360px;" />
        <span class="show-pwd" @click="showPwd">
          <svg-icon :icon-class="passwordType === 'password' ? 'eye' : 'eye-open'" />
        </span>
      </el-form-item>
      <el-form-item label="确认密码" prop="confirmPwd">
        <el-input v-model="form.confirmPwd" :type="passwordType" maxlength="20" auto-complete="on" style="width: 360px;" />
        <span class="show-pwd" @click="showPwd">
          <svg-icon :icon-class="passwordType === 'password' ? 'eye' : 'eye-open'" />
        </span>
      </el-form-item>
    </el-form>
    <template #footer>
      <div class="dialog-footer">
        <common-button type="text" @click="cancel">取消</common-button>
        <common-button :loading="loading" type="primary" @click="doSubmit">确认</common-button>
      </div>
    </template>
  </common-dialog>
</template>

<script setup>
import { updatePwd } from '@/api/user'
import { ref, defineProps, defineEmits } from 'vue'

import { ElMessage } from 'element-plus'
import useVisible from '@/composables/use-visible'

const emit = defineEmits(['update:modelValue'])

const props = defineProps({
  modelValue: {
    type: Boolean,
    require: true
  }
})

const { visible, handleClose } = useVisible({ emit, props })

const confirmPwd = (rule, value, callback) => {
  if (value) {
    if (form.value.newPwd !== value) {
      callback(new Error('两次输入的密码不一致'))
    } else if (form.value.oldPwd.length >= 6 && form.value.oldPwd === form.value.newPwd) {
      callback(new Error('新密码与旧密码一致'))
    } else {
      callback()
    }
  } else {
    callback(new Error('请再次输入新密码'))
  }
}

const rules = {
  oldPwd: [
    { required: true, message: '请填写旧密码', trigger: 'blur' }
  ],
  newPwd: [
    { required: true, message: '请填写新密码', trigger: 'blur' },
    { min: 6, max: 20, message: '长度在 6 到 20 个字符', trigger: 'blur' }
  ],
  confirmPwd: [
    { required: true, validator: confirmPwd, trigger: 'blur' }
  ]
}

const formRef = ref()
const loading = ref(false)
const passwordType = ref('password')
const form = ref({
  oldPwd: '',
  newPwd: '',
  confirmPwd: ''
})

// 关闭
function cancel() {
  resetForm()
  handleClose()
}

// 提交
function doSubmit() {
  formRef.value.validate((valid) => {
    if (valid) {
      loading.value = true
      updatePwd(form.value).then(res => {
        ElMessage.success('密码修改成功')
        cancel()
      }).catch(err => {
        console.log(err.response.data.message)
      }).finally(() => {
        loading.value = false
      })
    } else {
      return false
    }
  })
}

// 重置
function resetForm() {
  formRef.value.resetFields()
  form.value = { oldPwd: '', newPwd: '', confirmPwd: '' }
}

// 切换文本类型
function showPwd() {
  passwordType.value = passwordType.value === 'password' ? '' : 'password'
}
</script>

<style scoped>
.show-pwd {
  position: relative;
  right: 24px;
  cursor: pointer;
  padding: 0 3px;
}
</style>
