<template>
  <common-dialog
    append-to-body
    :close-on-click-modal="false"
    :before-close="crud.cancelCU"
    :visible="crud.status.cu > 0"
    :title="crud.status.title"
    width="500px"
  >
    <template #titleRight>
      <common-button :loading="crud.status.cu === 2" type="primary" size="mini" @click="crud.submitCU">确认</common-button>
    </template>
    <el-form ref="formRef" :model="form" :rules="rules" size="small" label-width="90px">
      <el-form-item label="工序" prop="processId">
        <process-select v-model="form.processId" :productType="productType" containsMachinePart :size="'small'" :multiple="false" style="width: 270px" />
      </el-form-item>
      <el-form-item label="班组属性" prop="organizationType">
        <el-select v-model="form.organizationType" placeholder="请选择班组属性" :size="'small'" style="width: 270px">
          <el-option v-for="item in teamAttributeEnum.ENUM" :key="item.V" :label="item.L" :value="item.V" />
        </el-select>
      </el-form-item>
      <el-form-item label="组长" prop="leaderId">
        <user-select
          ref="leaderSelectRef"
          v-model="form.leaderId"
          :size="'small'"
          :disabled-value="[...form.memberIds]"
          placeholder="请选择班组组长"
          style="width: 270px"
          @change="leaderChange"
        />
      </el-form-item>
      <el-form-item label="组员" prop="memberIds">
        <user-select
          ref="memberSelectRef"
          v-model="form.memberIds"
          :size="'small'"
          :disabled-value="[form.leaderId]"
          placeholder="请选择班组成员"
          :multiple="true"
          style="width: 270px"
          @change="memberChange"
        />
      </el-form-item>
    </el-form>
  </common-dialog>
</template>

<script setup>
import { ref, defineProps } from 'vue'
import { regForm } from '@compos/use-crud'
import processSelect from '@comp-mes/process-select'
import userSelect from '@comp-common/user-select'
import { teamAttributeEnum } from '@enum-ms/mes'

defineProps({
  productType: {
    type: Number,
    default: undefined
  }
})

const formRef = ref()
const leaderSelectRef = ref()
const memberSelectRef = ref()

const defaultForm = {
  id: undefined,
  processId: undefined,
  leaderId: undefined,
  organizationType: undefined,
  memberIds: []
}

const { crud, form } = regForm(defaultForm, formRef)

const rules = {
  processId: [{ required: true, message: '请选择工序', trigger: 'change' }],
  organizationType: [{ required: true, message: '请选择班组属性', trigger: 'change' }],
  leaderId: [{ required: true, message: '请选择组长', trigger: 'change' }],
  memberIds: [{ required: true, message: '请选择组员', trigger: 'change' }]
}

function leaderChange(userlist) {
  form.leader = userlist
}
function memberChange(userlist) {
  form.members = userlist
}
</script>
