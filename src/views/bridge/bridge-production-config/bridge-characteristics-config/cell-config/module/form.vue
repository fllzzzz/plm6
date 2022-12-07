<template>
  <common-drawer
    append-to-body
    :close-on-click-modal="false"
    :before-close="crud.cancelCU"
    :visible="crud.status.cu > 0"
    :title="crud.status.title"
    :wrapper-closable="false"
    size="650px"
  >
    <template #titleRight>
      <common-button :loading="crud.status.cu === 2" type="primary" size="mini" @click="crud.submitCU">确认</common-button>
    </template>
    <template #content>
      <el-form ref="formRef" :model="form" :rules="rules" size="small" label-width="140px" style="margin">
        <el-form-item label="类型命名" prop="name">
          <el-input v-model.trim="form.name" type="text" placeholder="类型命名" style="width: 270px" maxlength="30" />
        </el-form-item>
        <el-form-item label="排序" prop="sort">
          <el-input-number v-model.number="form.sort" :min="1" :max="999" :step="1" controls-position="right" style="width: 270px" />
        </el-form-item>
        <el-form-item label="规格前缀" prop="list">
          <div class="process-container">
            <div class="process-box">
              <div v-for="(item, index) in form.list" :key="index" class="process-drawer">
                <el-input
                  v-model="item.specPrefix"
                  type="text"
                  placeholder="规格前缀"
                  style="width: 270px; margin-right: 5px"
                  @blur="checkName(item, index)"
                />
                <common-button
                  v-show="form.list && form.list.length > 1"
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
import { ref } from 'vue'
import { ElMessage } from 'element-plus'

import { regForm } from '@compos/use-crud'

const formRef = ref()
const nameArr = ref([])
const defaultForm = {
  id: undefined,
  name: '',
  sort: undefined,
  list: []
}

const { crud, form, CRUD } = regForm(defaultForm, formRef)

const validateLinks = (rule, value, callback) => {
  if (value && value.length) {
    for (const i in value) {
      if (!value[i].specPrefix) {
        callback(new Error('请填写规格前缀'))
      } else {
        callback()
      }
    }
    callback()
  } else {
    callback(new Error('请填写规格前缀'))
  }
}

const rules = {
  name: [
    { required: true, message: '请填写类型名称', trigger: 'blur' },
    { min: 1, max: 30, message: '长度在 1 到 30 个字符', trigger: 'blur' }
  ],
  list: [
    { required: true, message: '请填写规格前缀' },
    { validator: validateLinks, trigger: 'change' }
  ]
}

function addProcess() {
  form.list.push({
    specPrefix: undefined
  })
}
function delProcess(index) {
  form.list.splice(index, 1)
}

function checkName(item, index) {
  const val = nameArr.value.find((v) => v.index === index)
  if (val) {
    if (item.specPrefix) {
      if (val.specPrefix === item.specPrefix) {
        return
      }
      if (nameArr.value.findIndex((v) => v.specPrefix === item.specPrefix) > -1) {
        ElMessage({
          message: '规格前缀已存在，请重新填写',
          type: 'error'
        })
        item.specPrefix = undefined
        val.specPrefix = undefined
      } else {
        val.specPrefix = item.specPrefix
      }
    } else {
      val.specPrefix = undefined
    }
  } else {
    if (item.specPrefix) {
      if (nameArr.value.findIndex((v) => v.specPrefix === item.specPrefix) > -1) {
        ElMessage({
          message: '规格前缀已存在，请重新填写',
          type: 'error'
        })
        form.list[index].specPrefix = undefined
      }
      nameArr.value.push({
        specPrefix: item.specPrefix,
        index: index
      })
    }
  }
}

CRUD.HOOK.beforeSubmit = () => {
  form.elementSpecList = []
  form.list.map(v => {
    form.elementSpecList.push(v.specPrefix)
  })
}

CRUD.HOOK.beforeToAdd = () => {
  nameArr.value = []
}

CRUD.HOOK.beforeToEdit = () => {
  nameArr.value = []
  form.list.map((v, index) => {
    nameArr.value.push({
      specPrefix: v.specPrefix,
      index: index
    })
  })
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
.add-row-box {
  text-align: center;
}
</style>
