<template>
  <common-dialog ref="dialogRef" title="套料设定" v-model:visible="dialogVisible" :before-close="handleClose" width="400px">
    <template #titleLeft>
      <el-tag>单位：mm</el-tag>
    </template>
    <template #titleRight>
      <common-button v-loading.fullscreen.lock="fullscreenLoading" type="success" size="mini" @click="submitForm(formRef)">
        开始套料
      </common-button>
    </template>
    <el-form ref="formRef" :model="form" :rules="rules" size="small" label-width="120px" class="demo-form">
      <el-form-item label="预留割缝≤：" prop="kerfLength">
        <el-input v-model="form.kerfLength" style="width: 200px" placeholder="输入长度 单位：mm" />
      </el-form-item>
      <el-form-item label="母材长度≤：" prop="length">
        <el-input v-model="form.length" style="width: 200px" placeholder="输入长度 单位：mm" />
      </el-form-item>
      <el-form-item label="材料属性：" prop="typesettingAssembleTypeEnum">
        <span>{{ materialTypeEnum.VL[props.detailData[0]?.typesettingAssembleTypeEnum] }}</span>
      </el-form-item>
      <el-form-item label="套料方式" prop="typesettingTypeEnum">
        <template #label>
          套料方式
          <el-tooltip
            effect="dark"
            :content="`如焊接型部件，可选择无损套料，选择无损套料将会按照约定的长度内进行套料\n
              如型材部件，可选择无损套料，选择有损套料，可能会产生一定的材料损耗`"
            placement="bottom"
          >
            <i class="el-icon-info" />
          </el-tooltip>
        </template>
        <common-radio
          class="filter-item"
          v-model="form.typesettingTypeEnum"
          :options="nestingSettingTypeEnum.ENUM"
          type="enum"
          size="small"
        />
      </el-form-item>
    </el-form>
  </common-dialog>
  <nesting-progress v-model:visible="extrusionVisible" :batchId="batchId" @success="crud.toQuery" />
</template>

<script setup>
import { defineProps, ref, defineEmits, reactive, inject } from 'vue'
import { nestingSettingTypeEnum, mesBuildingTypeSettingAssembleTypeEnum as materialTypeEnum } from '@enum-ms/mes'
import useVisible from '@compos/use-visible'
import { extrusionNesting } from '@/api/mes/craft-manage/section-steel/nesting-setting'
import nestingProgress from './nesting-progress.vue'

const formRef = ref()
const dialogRef = ref()
const crud = inject('crud')
const fullscreenLoading = ref(false)
const extrusionVisible = ref(false)
const batchId = ref()
const props = defineProps({
  visible: {
    type: Boolean,
    default: false
  },
  projectId: {
    type: [String, Number],
    default: undefined
  },
  detailData: {
    type: Array,
    default: () => []
  },
  monomerId: {
    type: Number
  }
})
const emit = defineEmits(['update:visible'])
const { visible: dialogVisible, handleClose } = useVisible({ emit, props, field: 'visible' })

const form = reactive({
  kerfLength: 20, // 预留割缝
  length: 12000, // 母材长度
  typesettingTypeEnum: undefined // 套料方式
})
const rules = {
  kerfLength: [{ required: true, message: '请输入预留割缝', trigger: 'blur' }],
  length: [{ required: true, message: '请输入母材长度', trigger: 'blur' }],
  typesettingTypeEnum: [{ required: true, message: '请选择套料方式', trigger: 'blur' }]
}

async function submitForm(formRef) {
  try {
    const _data = []
    const _content = []
    props.detailData.map((v) => {
      _data.push({
        id: v.assembleDetailId,
        quantity: v.quantity
      })
      _content.push(v.productionLineTypeEnum)
    })
    const _list = {
      assembleSettingList: _data,
      projectId: props.projectId,
      monomerId: props.monomerId,
      length: form.length,
      kerfLength: form.kerfLength,
      productionLineTypeEnum: _content[0],
      typesettingTypeEnum: form.typesettingTypeEnum
    }
    batchId.value = await extrusionNesting(_list)
    handleClose()
    fullscreenLoading.value = true
    setTimeout(() => {
      fullscreenLoading.value = false
    }, 2000)
    crud.toQuery()
    extrusionVisible.value = true
  } catch (err) {
    console.log('套料失败')
  }
}
</script>
