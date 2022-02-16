<template>
  <common-drawer
    append-to-body
    :close-on-click-modal="false"
    :before-close="handleClose"
    v-model="visible"
    title="特殊零件标记"
    :wrapper-closable="false"
    size="860px"
  >
    <template #titleRight>
      <common-button type="primary" size="mini" @click="onSubmit">确认</common-button>
    </template>
    <template #content>
      <el-form ref="formRef" :model="form" :rules="rules" size="small" label-width="140px">
        <el-form-item label="标记明细(大写)" prop="list">
          <div class="process-container">
            <div class="process-box">
              <div v-for="(item, index) in form.list" :key="index" class="process-drawer">
                <el-input v-model="form.list[index]" type="text" placeholder="请填写大写字母标记" style="width: 200px" maxlength="10" @blur="getName(form.list[index],index)"/>
                <common-button
                  v-show="form.list && form.list.length > 0"
                  icon="el-icon-delete"
                  size="mini"
                  type="danger"
                  style="margin-left: 6px"
                  @click="delProcess(index)"
                />
              </div>
            </div>
            <common-button icon="el-icon-plus" size="mini" type="success" style="margin: 0 0 12px 6px" @click="addProcess" />
          </div>
        </el-form-item>
      </el-form>
    </template>
  </common-drawer>
</template>

<script setup>
import { ref, defineEmits, watch, defineProps, nextTick } from 'vue'
import useVisible from '@compos/use-visible'
import useWatchFormValidate from '@compos/form/use-watch-form-validate'
import crudApi from '@/api/config/system-config/machine-part-config'
import { ElMessage } from 'element-plus'

const formRef = ref()
const defaultForm = {
  list: []
}
const form = ref(JSON.parse(JSON.stringify(defaultForm)))
const props = defineProps({
  modelValue: {
    type: Boolean,
    require: true
  },
  list: {
    type: Array,
    default: () => []
  }
})

watch(
  () => props.modelValue,
  (val) => {
    if (val) {
      resetForm()
    }
  },
  { deep: true, immediate: true }
)

const emit = defineEmits(['success', 'update:modelValue'])
const { visible, handleClose } = useVisible({ emit, props })

const validateLinks = (rule, value, callback) => {
  if (value && value.length > 0) {
    for (const i in value) {
      if (!value[i]) {
        callback(new Error('请输入大写字母标记'))
      }
    }
  }
  callback()
}

function getName(val, index) {
  if (val && !/^[A-Z]+$/.test(val)) {
    form.value.list[index] = undefined
  }
}

const rules = {
  list: [{ validator: validateLinks, trigger: 'change' }]
}
function resetForm() {
  if (formRef.value) {
    formRef.value.resetFields()
  }
  form.value.list = []
  if (props.list && props.list.length > 0) {
    props.list.forEach(v => {
      form.value.list.push(v)
    })
  }
  if (formRef.value) {
    nextTick(() => {
      formRef.value.clearValidate()
    })
  }
  useWatchFormValidate(formRef, form)
}

function addProcess() {
  form.value.list.push(undefined)
}
function delProcess(index) {
  form.value.list.splice(index, 1)
}

async function onSubmit() {
  try {
    const valid = await formRef.value.validate()
    if (valid) {
      await crudApi.add({ productType: 1, specificationLetters: form.value.list })
      ElMessage.success('添加成功')
      emit('success')
      handleClose()
    }
  } catch (e) {
    console.log('添加特殊零件标记', e)
  }
}
</script>
<style lang="scss" scoped>
::v-deep(.el-input-number .el-input__inner) {
  text-align: left;
}
.process-container {
  display: flex;
  flex-direction: row;
  justify-content: flex-start;
  align-items: flex-end;
  .process-box {
    display: flex;
    flex-direction: column;
    justify-content: flex-start;
    align-items: flex-start;
    .process-drawer {
      display: flex;
      flex-direction: row;
      justify-content: flex-start;
      align-items: center;
      margin-bottom: 10px;
    }
  }
}
</style>
