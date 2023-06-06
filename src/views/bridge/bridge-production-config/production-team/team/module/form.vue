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
      <el-form-item label="工序" prop="processId">
        <process-select
          ref="processSelectRef"
          v-model="form.processId"
          containsMachinePart
          :disabled="isEdit"
          :size="'small'"
          :multiple="false"
          style="width: 270px"
          @change="processChange"
        />
      </el-form-item>
      <!-- <el-form-item label="班组属性" prop="organizationType">
        <el-select v-model="form.organizationType" placeholder="请选择班组属性" :size="'small'" style="width: 270px" :disabled="isEdit">
          <el-option v-for="item in teamAttributeEnum.ENUM" :key="item.V" :label="item.L" :value="item.V" />
        </el-select>
      </el-form-item> -->
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
      <!-- <el-form-item label="工资单列" prop="boolExtraCountEnum">
        <common-radio v-model="form.boolExtraCountEnum" :options="whetherEnum.ENUM" type="enum" />
      </el-form-item> -->
      <!-- <el-form-item label="计价方式" prop="wageQuotaType" v-if="showWageQuotaTypeEnum?.length">
        <common-select v-model="form.wageQuotaType" :options="showWageQuotaTypeEnum" type="enum" style="width: 270px" />
      </el-form-item> -->
    </el-form>
  </common-dialog>
</template>

<script setup>
import { ref, computed } from 'vue'
import { regForm } from '@compos/use-crud'
import factorySelect from '@comp-base/factory-select.vue'
import processSelect from '@/components-system/bridge/process-select'
import userSelect from '@comp-common/user-select'
// import { wageQuotaTypeMap } from '@/settings/config'
// import { whetherEnum } from '@enum-ms/common'
import { teamAttributeEnum } from '@enum-ms/mes'
// import EO from '@enum'

const formRef = ref()
const processSelectRef = ref()
const leaderSelectRef = ref()
const memberSelectRef = ref()
const productType = ref()

const defaultForm = {
  id: undefined,
  processId: undefined,
  leaderId: undefined,
  organizationType: teamAttributeEnum.IN_STAFF.V,
  // boolExtraCountEnum: false,
  memberIds: []
}

const { crud, form } = regForm(defaultForm, formRef)
const isEdit = computed(() => crud.status.edit >= 1)

const rules = {
  factoryId: [{ required: true, message: '请选择工厂', trigger: 'change' }],
  processId: [{ required: true, message: '请选择工序', trigger: 'change' }],
  // organizationType: [{ required: true, message: '请选择班组属性', trigger: 'change' }],
  leaderId: [{ required: true, message: '请选择组长', trigger: 'change' }],
  memberIds: [{ required: true, message: '请选择组员', trigger: 'change' }]
  // boolExtraCountEnum: [{ required: true, message: '请选择工资是否单列', trigger: 'change', type: 'boolean' }],
  // wageQuotaType: [{ required: true, message: '请选择计价方式', trigger: 'change' }]
}

// const showWageQuotaTypeEnum = computed(() => {
//   const _type = wageQuotaTypeMap[productType.value]
//   return _type && EO.getBits(wageQuotaTypeEnum, _type)
// })

function processChange(val) {
  productType.value = processSelectRef.value?.getOption(val)?.productType
}

function leaderChange(userlist) {
  form.leader = userlist
}
function memberChange(userlist) {
  form.members = userlist
}
</script>
