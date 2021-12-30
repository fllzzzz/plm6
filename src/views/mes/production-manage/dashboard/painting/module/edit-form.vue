<template>
  <common-dialog title="涂装计算" v-model="dialogVisible" width="450px" :before-close="handleClose">
    <template #titleRight>
      <common-button :loading="saveLoading" :disabled="submitDisabled" type="primary" size="mini" @click="save"> 保 存 </common-button>
    </template>
    <el-form ref="formRef" :model="form" size="small" label-width="120px">
      <el-form-item label="名称" prop="name">
        <span v-empty-text>{{ form.name }}</span>
      </el-form-item>
      <el-form-item label="材质" prop="material">
        <span v-empty-text>{{ form.material }}</span>
      </el-form-item>
      <el-form-item label="油漆类别" prop="paintCategory">
        <el-input v-model="form.paintCategory" size="small" placeholder="请输入油漆类别" />
      </el-form-item>
      <el-form-item label="干膜厚度(μm)" prop="thickness">
        <el-input-number
          v-model="form.thickness"
          :step="1"
          :min="0"
          :precision="DP.COM_T__MM"
          size="small"
          style="width: 100%"
          controls-position="right"
          placeholder="请输入干膜厚度"
        />
      </el-form-item>
      <el-form-item label="体积固体份(%)" prop="volumeSolids">
        <el-input-number
          v-model="form.volumeSolids"
          :step="1"
          :min="0"
          :precision="2"
          size="small"
          style="width: 100%"
          controls-position="right"
          placeholder="请输入体积固体份"
        />
      </el-form-item>
      <el-form-item label="损耗(%)" prop="loss">
        <el-input-number
          v-model="form.loss"
          :step="1"
          :min="0"
          :precision="2"
          size="small"
          style="width: 100%"
          controls-position="right"
          placeholder="请输入损耗"
        />
      </el-form-item>
      <el-form-item label="实际用量(L)">
        <span>{{ measure }}</span>
      </el-form-item>
    </el-form>
  </common-dialog>
</template>

<script setup>
import { change } from '@/api/mes/production-manage/dashboard/painting'
import { defineEmits, defineProps, watch, computed, reactive, ref } from 'vue'
import { ElNotification } from 'element-plus'
import { deepClone } from '@data-type/index'

import { DP } from '@/settings/config'
import { convertUnits } from '@/utils/convert/unit'
import { toFixed } from '@data-type/index'
import { isObjectValueEqual } from '@data-type/object'

import useVisible from '@compos/use-visible'

const emit = defineEmits(['update:visible', 'refresh'])
const props = defineProps({
  visible: {
    type: Boolean,
    default: false,
  },
  info: {
    type: Object,
    default: () => {},
  },
})

const { visible: dialogVisible, handleClose } = useVisible({ emit, props, field: 'visible' })

let form = reactive({})
const saveLoading = ref(false)
const submitDisabled = computed(() => isObjectValueEqual(form, props.info))

const measure = computed(() => {
  // 面积*干膜厚度/（10*体积固体份*100*（1-损耗））
  return form.volumeSolids
    ? toFixed((form.changeArea * form.thickness) / (10 * (form.volumeSolids / 100) * 100 * (1 - form.loss / 100)), DP.COM_VOLUME__L)
    : 0
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
    _form.loss = _form.loss / 100
    _form.volumeSolids = _form.volumeSolids / 100
    await change(_form)
    ElNotification({ title: '修改成功', type: 'success' })
    handleClose()
    emit('refresh')
  } catch (error) {
    console.log('修改失败', error)
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
