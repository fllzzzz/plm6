<template>
  <common-dialog
    :title="`${props.detail.name}调整`"
    append-to-body
    v-model="visible"
    width="440px"
    show-close
    :before-close="handleClose"
    top="10vh"
  >
    <template #titleRight>
      <common-button  size="mini" type="primary" @click.stop="submit">确认</common-button>
    </template>
    <el-form ref="formRef" v-loading="submitLoading" :disabled="submitLoading" :model="form" :rules="rules" size="small" label-width="80px">
      <el-form-item label="名称">
        {{ props.detail.name }}
      </el-form-item>
      <el-form-item label="编号" prop="serialNumber">
        {{ props.detail.serialNumber }}
      </el-form-item>
      <el-form-item label="类型" prop="modifyType">
        <common-radio
          v-model="form.modifyType"
          :options="businessModifyTypeEnum.ENUM"
          type="enum"
        />
      </el-form-item>
      <el-form-item v-if="isSeparate" label="重新命名" prop="name">
        <el-input
          v-model="form.name"
          placeholder="请重新命名"
          size="small"
          class="input-underline"
          style="width: 300px;"
          clearable
        />
      </el-form-item>
      <el-form-item v-else label="合并" prop="businessId">
        <common-select
          v-model="form.businessId"
          :options="list"
          clearable
          filterable
          class="input-underline"
          placeholder="请选择"
          style="width: 300px;"
        >
        <template #view="{ data }">
          <span class="option-item">
            <span>{{ data.name }}</span>
            <span class="label">{{ data.material }}</span>
          </span>
        </template>
        </common-select>
      </el-form-item>
    </el-form>
  </common-dialog>
</template>

<script setup>
import { businessList, businessBind } from '@/api/contract/sales-manage/price-manage/structure'
import { ref, computed, watch, defineProps, defineEmits } from 'vue'

import { businessModifyTypeEnum } from '@enum-ms/contract'
import useVisible from '@compos/use-visible'

const emit = defineEmits(['success'])

const props = defineProps({
  detail: {
    type: Object,
    default: () => {}
  },
  structureId: {
    type: [String, Number],
    default: undefined
  },
  modelValue: {
    type: Boolean,
    require: true
  }
})

const { visible, handleClose } = useVisible({ emit, props })

// 是否单独计价
const isSeparate = computed(() => {
  return form.value.modifyType === businessModifyTypeEnum.SEPARATE.V
})

// 规则
const rules = computed(() => {
  const rules = {}
  if (isSeparate.value) {
    rules.name = [{ required: true, message: '请填重新命名', trigger: 'blur' }]
  } else {
    rules.businessId = [{ required: true, message: '请选择', trigger: 'change' }]
  }
  return rules
})

watch(
  () => props.modelValue,
  (val) => {
    if (val) {
      init()
      fetchBusinessList()
    }
  },
  { deep: true, immediate: true }
)

const formData = {
  name: '',
  businessId: undefined,
  modifyType: businessModifyTypeEnum.SEPARATE.V
}
const formRef = ref()
const list = ref([])
const form = ref({ ...formData })
const submitLoading = ref(false)

// 提交
function submit() {
  formRef.value.validate(async (valid) => {
    if (valid) {
      const param = {
        productId: props.detail.id
      }
      if (isSeparate.value) {
        param.name = form.value.name
      } else {
        param.businessId = form.value.businessId
      }
      await businessBind(param)
      emit('success')
      handleClose()
    }
    return false
  })
}

// 初始化数据
function init() {
  form.value = { ...formData }
}

// 获取商务构件列表
async function fetchBusinessList() {
  let _list = []
  try {
    const monomerId = props.detail.monomerId
    if (monomerId) {
      const { content = [] } = await businessList({ monomerId: props.detail.monomerId })
      _list = content.filter(v => v.id !== props.structureId)
    }
  } catch (error) {
    console.log('获取商务构件列表失败', error)
  } finally {
    list.value = _list
  }
}
</script>

<style lang="scss" scoped>
.option-item {
  width: 100%;
  display: inline-flex;
  justify-content: space-between;
}

.option-item > span:first-child {
  flex: none;
  margin-right: 15px;
}
.option-item > span:last-child {
  color: #8492a6;
  font-size: 13px;
}
</style>
