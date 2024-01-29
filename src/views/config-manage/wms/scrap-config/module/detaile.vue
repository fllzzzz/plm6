<template>
  <common-dialog v-model="visible" :before-close="handleClose" width="30%" :title="props.title">
    <template #titleRight>
      <common-button type="primary" @click="save" size="mini">保存</common-button>
    </template>
    <div style="display: flex; justify-content: center; align-items: center; flex-direction: column">
      <el-form>
        <el-form-item label="废料类型">
          <el-input style="width: 200px;" v-model="form.name" />
        </el-form-item>
        <el-form-item label="核算单位">
          <el-input style="width: 200px;" v-model="form.measureUnit" />
        </el-form-item>
      </el-form>
    </div>
  </common-dialog>
</template>
<script setup>
import { defineProps, defineEmits, ref } from 'vue'
import useVisible from '@compos/use-visible'
import { addScrapType, editScrapType } from '@/api/config/wms/scrap-config'

const props = defineProps({
  modelValue: {
    type: Boolean,
    default: false
  },
  rowData: {
    type: Object,
    default: () => {}
  },
  title: {
    type: String,
    default: '创建废料类型'
  }
})
const emit = defineEmits(['update:modelValue', 'success'])

const { visible, handleClose } = useVisible({ props, emit, showHook: fetchData })

const form = ref({
  id: undefined,
  name: undefined,
  measureUnit: undefined
})

async function fetchData() {
  if (props.title === '创建废料类型') {
    form.value.name = undefined
    form.value.measureUnit = undefined
    form.value.id = undefined
  } else {
    form.value.name = props.rowData.name
    form.value.id = props.rowData.id
    form.value.measureUnit = props.rowData.measureUnit
  }
}

const save = async () => {
  const _list = []

  _list.push(form.value)
  try {
    if (props.title === '创建废料类型') {
      await addScrapType(_list)
    } else {
      await editScrapType(_list)
    }
    emit('success')
    handleClose()
  } catch (error) {
    console.log(error)
  }
}

</script>
