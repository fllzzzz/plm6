<template>
  <common-drawer :visible="crud.status.cu > 0" :before-close="crud.cancelCU" :title="crud.status.title" :show-close="true" size="40%">
    <template #content>
      <el-form ref="formRef" :model="crud.form" :rules="rules" size="small" label-width="80px">
          <el-form-item label="规格名称" prop="name">
            <el-input
              v-model="crud.form.name"
              type="text"
              placeholder="请填写规格名称"
              size="small"
              maxlength="10"
              class="input-underline"
              style="width: 350px;"
            />
          </el-form-item>
      </el-form>
    </template>
  </common-drawer>
</template>

<script setup>
import { inject, ref } from 'vue'
import { useRegForm } from '@compos/use-crud'
const currentNode = inject('currentNode')

const defaultForm = {
  name: '', // 规格名称
  classificationId: currentNode.value.id, // 科目id
  isWeightMean: 1 // 是否参加加权平均
}

const rules = []

const formRef = ref()

const { crud } = useRegForm(defaultForm, formRef)
</script>
