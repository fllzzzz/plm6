<template>
  <common-dialog
    append-to-body
    :close-on-click-modal="false"
    :before-close="crud.cancelCU"
    :visible="crud.status.cu > CRUD.STATUS.NORMAL"
    :title="crud.status.title"
    width="500px"
  >
    <template #titleRight>
      <common-button :loading="crud.status.cu === CRUD.STATUS.PROCESSING" size="mini" type="primary" @click="crud.submitCU">
        确认
      </common-button>
    </template>
    <el-form ref="formRef" :model="form" :rules="rules" size="small" label-width="100px" class="tax-setting" @submit.prevent>
      <el-form-item label="种类" prop="name">
        <div>{{ form.name }}</div>
      </el-form-item>
      <el-form-item label="税率" prop="taxRateList">
        <div class="tax-list flex-rsc">
          <el-tag
            size="medium"
            v-for="(item, index) in form.taxRateList"
            :key="index"
            closable
            :disable-transitions="false"
            @close="handleClose(item)"
          >
            {{ item + '%' }}
          </el-tag>
          <common-input-number
            v-if="inputVisible"
            ref="inputRef"
            v-model="inputValue"
            class="input-new-tag"
            size="mini"
            :max="100"
            :min="0"
            :precision="2"
            :controls="false"
            placeholder="数字"
            @keyup.enter="handleInputConfirm"
            @blur="handleInputConfirm"
          />
          <common-button v-else class="button-new-tag" icon="el-icon-plus" size="mini" @click="showInput">添加</common-button>
        </div>
      </el-form-item>
    </el-form>
  </common-dialog>
</template>

<script setup>
import { ref, nextTick } from 'vue'
import { regForm } from '@compos/use-crud'
import { ElMessage } from 'element-plus'

const defaultForm = {
  name: '',
  taxRateList: []
}

const rules = {
  // taxRateList: [{ message: '请输入税率', trigger: 'change' }]
}

const formRef = ref()
const inputRef = ref()
const inputVisible = ref(false)
const inputValue = ref()

const { CRUD, crud, form } = regForm(defaultForm, formRef)

CRUD.HOOK.beforeToCU = () => {
  init()
}

function init() {
  inputVisible.value = false
  inputValue.value = undefined
}

function handleClose(tag) {
  form.taxRateList.splice(form.taxRateList.indexOf(tag), 1)
}

function showInput() {
  inputVisible.value = true
  nextTick(() => {
    inputRef.value.focus()
  })
}

function handleInputConfirm() {
  if (form.taxRateList.includes(inputValue.value)) {
    ElMessage.warning('当前税率已存在')
    return
  }
  if (inputValue.value) {
    form.taxRateList.push(inputValue.value)
  }
  init()
}
</script>

<style lang="scss" scoped>
.tax-setting {
  .el-tag {
    margin-right: 10px;
  }
  .button-new-tag {
    height: 32px;
    line-height: 30px;
    padding-top: 0;
    padding-bottom: 0;
    width: 90px;
  }
  .input-new-tag {
    width: 90px;
    vertical-align: bottom;
  }
  .tax-list {
    flex-wrap: wrap;
  }
  .tax-list > :nth-child(n) {
    margin-bottom: 10px;
  }
}
</style>
