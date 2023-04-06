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
    <el-form ref="formRef" :model="form" :rules="rules" size="small" label-width="100px">
      <el-form-item label="工厂" prop="factoryId">
        <factory-select :disabled="isEdit" v-model="form.factoryId" placeholder="请选择工厂" style="width: 270px" />
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
import { ref, computed } from 'vue'
import { regForm } from '@compos/use-crud'
import factorySelect from '@comp-base/factory-select.vue'
import userSelect from '@comp-common/user-select'

const formRef = ref()
const leaderSelectRef = ref()
const memberSelectRef = ref()

const defaultForm = {
  id: undefined,
  factoryId: undefined,
  leaderId: undefined,
  memberIds: []
}

const { crud, form } = regForm(defaultForm, formRef)
const isEdit = computed(() => crud.status.edit >= 1)

const rules = {
  factoryId: [{ required: true, message: '请选择工厂', trigger: 'change' }],
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
