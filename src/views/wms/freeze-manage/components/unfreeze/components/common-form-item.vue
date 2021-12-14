<template>
  <el-form-item label="对应单据">
    <span v-if="materialFreezeTypeEnum.V[record.freezeType]">{{ materialFreezeTypeEnum.V[record.freezeType].DOC }}</span>
  </el-form-item>
  <el-form-item label="单据编号">
    <span v-if="record.document">{{ record.document.serialNumber }}</span>
  </el-form-item>
  <el-form-item label="关联项目">
    <span v-parse-project="{ project: record.project, onlyShortName: true }" v-empty-text />
  </el-form-item>
  <el-form-item label="冻结数量">
    <span v-to-fixed="{ val: record.quantity, dp: record.outboundUnitPrecision }" />
    <span style="margin-left: 10px">{{ record.outboundUnit }}</span>
  </el-form-item>
  <el-form-item :label="`数量(${record.outboundUnit})`" prop="quantity">
    <div class="flex-rsc" style="width: 100%">
      <el-input-number
        v-model="currentForm.quantity"
        :min="0"
        :precision="record.outboundUnitPrecision"
        :max="record.corQuantity"
        controls-position="right"
        style="flex: auto"
      />
      <span class="text-clickable set-max-text" style="flex: none" @click="setMaxQuantity">全部解冻</span>
    </div>
  </el-form-item>
  <el-form-item label="备注" prop="remark">
    <el-input
      v-model.trim="currentForm.remark"
      type="textarea"
      :autosize="{ minRows: 3, maxRows: 3 }"
      maxlength="200"
      show-word-limit
      placeholder="备注"
      style="width: 100%"
    />
  </el-form-item>
</template>

<script setup>
import { defineProps, watchEffect, ref } from 'vue'
import { materialFreezeTypeEnum } from '@/utils/enum/modules/wms'

const props = defineProps({
  record: {
    // 物料出库信息
    type: Object
  },
  form: {
    // 物料出库表单
    type: Object
  }
})

// 表单
const currentForm = ref({
  // 当前表单字段
  quantity: undefined, // 数量
  remark: undefined // 备注
})
// 监听
watchEffect(() => {
  currentForm.value = props.form
})

// 设置最大数量
function setMaxQuantity() {
  currentForm.value.quantity = props.record.corQuantity
}
</script>

<style lang="scss" scoped>
.set-max-text {
  margin-left: 10px;
}
</style>
