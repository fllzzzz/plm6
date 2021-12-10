<template>
  <el-form-item :label="`数量(${material.outboundUnit})`" prop="quantity">
    <el-input-number
      v-model="currentForm.quantity"
      :min="0"
      :precision="material.outboundUnitPrecision"
      :max="material.operableQuantity"
      controls-position="right"
    />
    <span class="text-clickable set-max-text" @click="setMaxQuantity">全部出库</span>
  </el-form-item>
  <el-form-item v-if="showProjectSelect" label="项目" prop="projectId">
    <project-cascader v-model="currentForm.projectId" clearable style="width: 100%" />
  </el-form-item>
  <el-form-item label="领用人" prop="recipientId">
    <user-dept-cascader
      v-model="currentForm.recipientId"
      :collapse-tags="false"
      clearable
      filterable
      show-all-levels
      placeholder="领用人"
      style="width: 100%"
    />
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
import { defineProps, inject, computed, watchEffect, ref } from 'vue'
import userDeptCascader from '@comp-base/user-dept-cascader.vue'
import projectCascader from '@comp-base/project-cascader.vue'

const props = defineProps({
  material: {
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
  projectId: undefined, // 项目id
  recipientId: undefined, // 领用人id
  quantity: undefined, // 数量
  remark: undefined // 备注
})
// 监听
watchEffect(() => { currentForm.value = props.form })

// 出库配置
const outboundCfg = inject('outboundCfg')

// 显示项目选择 ：项目仓，且配置为可出库到其他项目的情况下可选择
const showProjectSelect = computed(() => {
  return !props.material.project || outboundCfg.value.boolCanOutToOtherProject === true
})

// 设置最大数量
function setMaxQuantity() {
  currentForm.value.quantity = props.material.corOperableQuantity
}
</script>

<style lang="scss" scoped>
.set-max-text {
  margin-left: 10px;
}
</style>
