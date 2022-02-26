<template>
  <common-drawer
    ref="drawerRef"
    :visible="crud.status.cu > CRUD.STATUS.NORMAL"
    :before-close="crud.cancelCU"
    :title="crud.status.title"
    :show-close="true"
    size="100%"
    :close-on-click-modal="false"
    custom-class="project-preparation-form"
  >
    <template #titleRight>
      <common-button :loading="crud.status.cu === CRUD.STATUS.PROCESSING" size="mini" type="primary" @click="crud.submitCU">
        提 交
      </common-button>
    </template>
    <template #content>
      <div class="main-content">
        <div class="head">
          <list-and-match />
        </div>
        <div class="middle">
          <el-form ref="formRef" :model="form" :rules="rules" size="small" label-position="top" inline label-width="200px"></el-form>
        </div>
      </div>
    </template>
  </common-drawer>
</template>

<script setup>
import { ref } from 'vue'

import ListAndMatch from './list-and-match'
import { regForm } from '@compos/use-crud'

const defaultForm = {}

const formRef = ref()

const { CRUD, crud, form } = regForm(defaultForm, formRef)

// 初始化表单
CRUD.HOOK.afterToAdd = () => {}

CRUD.HOOK.beforeToEdit = (crud, form) => {}

// 表单提交数据清理
crud.submitFormFormat = (form) => {
  return form
}
</script>

<style lang="scss" scoped>
.main-content {
  ::v-deep(.input-underline input) {
    text-align: left;
  }
}

.form-content {
  width: 750px;
  display: grid;
  grid-template-columns: repeat(12, 8.33%);
  grid-template-rows: auto;
  grid-column-gap: 20px;
  grid-auto-flow: row;
  grid-template-areas:
    'a a a a a a b b b b b b'
    'c c c c c c e e e e e e'
    'd d d d d d d d d d d d'
    'z z z z z z z z z z z z'
    'f f f f f f g g g g g g'
    'r r r r h h h h i i i i'
    'j j j j m m m m m m . .'
    'k k k k p p p p q q q q'
    'o o o o o o n n n n n n';
}
</style>
