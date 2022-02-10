<template>
  <common-dialog title="表面积修改" v-model="dialogVisible" width="400px" :before-close="handleClose">
    <template #titleRight>
      <common-button :loading="saveLoading" :disabled="form.changeArea === form.originChangeArea" type="primary" size="mini" @click="save">
        保 存
      </common-button>
    </template>
    <el-form ref="formRef" :model="form" size="small" label-width="100px">
      <el-form-item label="清单量(㎡)" prop="surfaceArea">
        <span>{{ toFixed(form.surfaceArea, DP.COM_AREA__M2) }}</span>
      </el-form-item>
      <el-form-item label="修改量(㎡)" prop="changeArea">
        <el-input-number
          v-model="form.changeArea"
          :step="1"
          :min="0"
          :precision="DP.COM_AREA__M2"
          size="small"
          style="width: 100%"
          @keydown.enter="save"
          controls-position="right"
          placeholder="请输入表面积"
        />
      </el-form-item>
      <el-form-item label="差异(㎡)" prop="diff">
        <span :style="{ color: diff > 0 ? '#11b95c' : 'red' }">{{
          diff > 0 ? ` ↓${toFixed(diff, DP.COM_AREA__M2)} ` : ` ↑${toFixed(-diff, DP.COM_AREA__M2)} `
        }}</span>
      </el-form-item>
    </el-form>
  </common-dialog>
</template>

<script setup>
import { areaChange } from '@/api/mes/production-manage/dashboard/painting'
import { defineEmits, defineProps, watch, computed, reactive, ref } from 'vue'
import { ElNotification } from 'element-plus'

import { deepClone } from '@data-type/index'
import { convertUnits } from '@/utils/convert/unit'
import { DP } from '@/settings/config'
import { toFixed } from '@data-type/index'

import useVisible from '@compos/use-visible'

const emit = defineEmits(['update:visible', 'refresh'])
const props = defineProps({
  visible: {
    type: Boolean,
    default: false
  },
  info: {
    type: Object,
    default: () => {}
  }
})

const { visible: dialogVisible, handleClose } = useVisible({ emit, props, field: 'visible' })

let form = reactive({})
const saveLoading = ref(false)
const diff = computed(() => {
  return form.surfaceArea - form.changeArea
})

watch(
  () => props.visible,
  (visible) => {
    if (visible) {
      form = Object.assign(form, props.info)
    }
  },
  { immediate: true }
)

async function save() {
  try {
    saveLoading.value = true
    const _form = deepClone(form)
    _form.changeArea = convertUnits(_form.changeArea, '㎡', 'mm²')
    const { projectId, monomerId, changeArea, paintingType, material, name } = _form
    await areaChange({ projectId, monomerId, changeArea, paintingType, material, name })
    ElNotification({ title: '修改成功', type: 'success' })
    handleClose()
    emit('refresh')
  } catch (error) {
    console.log('表面积修改失败', error)
  } finally {
    saveLoading.value = false
  }
}
</script>

<style rel="stylesheet/scss" lang="scss" scoped>
::v-deep(.el-input-number .el-input__inner) {
  text-align: left;
}
</style>
