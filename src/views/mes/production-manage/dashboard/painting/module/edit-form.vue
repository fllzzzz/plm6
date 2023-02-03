<template>
  <common-dialog title="涂装计算" v-model="dialogVisible" width="450px" :before-close="handleClose">
    <template #titleRight>
      <common-button :loading="saveLoading" type="primary" size="mini" @click="save"> 保 存 </common-button>
    </template>
    <el-form ref="formRef" :model="form" :rules="rules" size="small" label-width="120px">
      <el-form-item label="油漆类别">
        <span v-if="form.paintingType">{{ paintingTypeEnum.VL[form.paintingType] }}</span>
      </el-form-item>
      <el-form-item label="干膜厚度(μm)" prop="thickness">
        <common-input-number
          v-model="form.thickness"
          :step="1"
          :precision="DP.COM_T__MM"
          size="small"
          style="width: 100%"
          controls-position="right"
          placeholder="请输入干膜厚度"
        />
      </el-form-item>
      <el-form-item label="体积固体份(%)" prop="volumeSolids">
        <common-input-number
          v-model="form.volumeSolids"
          :step="1"
          :precision="2"
          size="small"
          style="width: 100%"
          controls-position="right"
          placeholder="请输入体积固体份"
        />
      </el-form-item>
      <el-form-item label="损耗(%)" prop="loss">
        <common-input-number
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
      <el-form-item label="应用全部单体" prop="applyAll">
        <el-checkbox v-model="form.applyAll" label="全部单体" />
      </el-form-item>
      <!-- <el-form-item label="实际用量(L)">
        <span>{{ measure }}</span>
      </el-form-item> -->
    </el-form>
  </common-dialog>
</template>

<script setup>
import { change } from '@/api/mes/production-manage/dashboard/painting'
import { defineEmits, defineProps, watch, reactive, ref } from 'vue'
import { ElNotification } from 'element-plus'
import { deepClone } from '@data-type/index'

import { paintingTypeEnum } from '@enum-ms/mes'
import { DP } from '@/settings/config'
// import { toFixed } from '@data-type/index'
import { convertUnits } from '@/utils/convert/unit'

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
const formRef = ref()
const saveLoading = ref(false)

const validateData = (rule, value, callback) => {
  if (!value) {
    callback(new Error('填写数据必须大于0'))
  }
  callback()
}
const rules = ref({
  thickness: [{ required: true, validator: validateData, trigger: 'blur' }],
  volumeSolids: [{ required: true, validator: validateData, trigger: 'blur' }]
})
// const measure = computed(() => {
//   // 面积*干膜厚度/（10*体积固体份*100*（1-损耗））
//   return form.volumeSolids
//     ? toFixed((form.surfaceArea * form.thickness) / (10 * (form.volumeSolids / 100) * 100 * (1 - form.loss / 100)), DP.COM_VOLUME__L)
//     : 0
// })

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
    const valid = await formRef.value.validate()
    if (!valid) return false
    const _form = deepClone(form)
    _form.surfaceArea = convertUnits(_form.surfaceArea, '㎡', 'mm²')
    _form.loss = _form.loss / 100
    _form.volumeSolids = _form.volumeSolids / 100
    if (form.applyAll) {
      delete _form.monomerId
    }
    await change(_form)
    ElNotification({ title: '保存成功', type: 'success' })
    handleClose()
    emit('refresh')
  } catch (error) {
    console.log('保存失败', error)
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
