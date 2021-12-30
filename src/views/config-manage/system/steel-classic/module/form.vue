<template>
  <common-drawer
    append-to-body
    :close-on-click-modal="false"
    :before-close="crud.cancelCU"
    :visible="crud.status.cu > 0"
    :title="crud.status.title"
    :wrapper-closable="false"
    size="860px"
  >
    <template #titleRight>
      <common-button :loading="crud.status.cu === 2" type="primary" size="mini" @click="crud.submitCU">确认</common-button>
    </template>
    <template #content>
      <el-form ref="formRef" :model="form" :rules="rules" size="small" label-width="140px">
        <el-form-item label="钢材分类名称" prop="name">
          <el-input v-model="form.name" type="text" placeholder="钢材分类名称" style="width: 270px" maxlength="30"/>
        </el-form-item>
        <el-form-item label="排序" prop="sort">
          <el-input-number v-model.number="form.sort" :min="1" :max="999" :step="1" controls-position="right" style="width: 270px" />
        </el-form-item>
        <el-form-item label="关键字母" prop="links">
          <div class="process-container">
            <div class="process-box">
              <div v-for="(item, index) in form.links" :key="index" class="process-drawer">
                <el-input v-model="item.keyword" type="text" placeholder="大写字母" style="width: 270px" oninput="value=value.replace(/[^/A-Z]/g,'')" @change="checkName(item,index)"/>
                <common-select
                  v-model="item.specIndex"
                  :options="specIndexEnum"
                  showOptionAll
                  :allVal="0"
                  type="enum"
                  size="small"
                  clearable
                  class="filter-item"
                  placeholder="索引"
                  style="width: 250px"
                />
                <common-button
                  v-show="form.links && form.links.length > 1"
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
import { regForm } from '@compos/use-crud'
import { ElMessage } from 'element-plus'
import { isNotBlank } from '@data-type/index'

const formRef = ref()
const nameArr = ref([])
const specIndexEnum = {
  1: { L: '1', K: '1', V: 1 },
  2: { L: '2', K: '2', V: 2 },
  3: { L: '3', K: '3', V: 3 },
  4: { L: '4', K: '4', V: 4 }
}
const defaultForm = {
  iid: undefined,
  name: '',
  sort: 1,
  links: []
}

const { crud, form, CRUD } = regForm(defaultForm, formRef)
const validateLinks = (rule, value, callback) => {
  if (value && value.length) {
    for (const i in value) {
      if (!value[i].keyword) {
        callback(new Error('请填写关键字母'))
      }
      if (!isNotBlank(value[i].specIndex)) {
        callback(new Error('请选择索引'))
      }
    }
    callback()
  } else {
    callback(new Error('请填写关键字母'))
  }
}

const rules = {
  name: [
    { required: true, message: '请填写钢材分类名称', trigger: 'blur' },
    { min: 1, max: 30, message: '长度在 1 到 30 个字符', trigger: 'blur' }
  ],
  sort: [{ required: true, message: '请填写排序值', trigger: 'blur', type: 'number' }],
  links: [
    { required: true, message: '请选择关键字母' },
    { validator: validateLinks, trigger: 'change' }
  ]
}

function addProcess() {
  form.links.push({
    keyword: undefined,
    specIndex: undefined
  })
}
function delProcess(index) {
  form.links.splice(index, 1)
}

function checkName(item, index) {
  if (item.keyword) {
    const val = nameArr.value.find(v => v.index === index)
    if (nameArr.value.findIndex(v => v.keyword === item.keyword) > -1) {
      ElMessage({
        message: '关键字母已存在，请重新填写',
        type: 'error'
      })
      item.keyword = undefined
      if (val) {
        val.keyword = undefined
      }
    } else {
      if (val) {
        val.keyword = item.keyword
      } else {
        nameArr.value.push({
          keyword: item.keyword,
          index: index
        })
      }
    }
  }
}

CRUD.HOOK.afterAddSuccess = () => {
  nameArr.value = []
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
