<template>
  <common-drawer
    ref="drawerRef"
    title="仓库名称"
    :close-on-click-modal="false"
    v-model="visible"
    direction="rtl"
    :before-close="handleClose"
    custom-class="workshop-form"
    size="800px"
  >
    <template #titleRight>
      <common-button icon="el-icon-plus" type="warning" @click="addItem" size="mini"/>
      <common-button size="mini" type="primary" @click="onSubmit" v-loading="submitLoading">提交</common-button>
    </template>
    <template #content>
      <common-table :data="list" :cell-class-name="wrongCellMask" :max-height="maxHeight" return-source-data :show-empty-symbol="false">
        <el-table-column label="序号" type="index" align="center" width="60" />
        <el-table-column key="type" prop="type" label="类型" align="center">
          <template v-slot="scope">
            <common-select
              v-model="scope.row.type"
              :options="warehouseTypeEnum.ENUM"
              type="enum"
              placeholder="类型"
              style="width: 100%"
            />
        </template>
        </el-table-column>
        <el-table-column key="name" prop="name" label="仓库名称" align="center">
          <template v-slot="scope">
            <workshop-select
              v-if="scope.row.type===warehouseTypeEnum.WORKSHOP.V"
              v-model="scope.row.workshopId"
              placeholder="请选择"
              :choseIndex="scope.$index"
              :disabledVal="disabledVal"
              @nameChange="workshopChange"
              style="width: 100%"
            />
            <el-input v-else v-model.trim="scope.row.name" type="text" clearable placeholder="名称" size="small" style="width: 100%" />
          </template>
        </el-table-column>
        <el-table-column label="操作" width="130px" align="center" fixed="right">
        <template v-slot="scope">
          <common-button icon="el-icon-delete" type="danger" size="mini" @click="deleteItem(scope.$index)" />
        </template>
      </el-table-column>
      </common-table>
    </template>
  </common-drawer>
</template>

<script setup>
import { addWorkshop } from '@/api/config/wms/warehouse'
import { ref, defineEmits, defineProps, watch, computed } from 'vue'

import { useStore } from 'vuex'
import { warehouseTypeEnum } from '@enum-ms/wms'
import { isNotBlank } from '@data-type/index'
import useTableValidate from '@compos/form/use-table-validate'
import useWorkshopName from '@compos/store/use-workshop-name'

import useVisible from '@compos/use-visible'
import useMaxHeight from '@compos/use-max-height'
import { ElNotification } from 'element-plus'
import workshopSelect from '@comp-base/workshop-select.vue'

const emit = defineEmits(['update:modelValue', 'success'])
const { workshopName } = useWorkshopName()

const props = defineProps({
  modelValue: {
    type: Boolean,
    require: true
  }
})

const { visible, handleClose } = useVisible({ emit, props })
const submitLoading = ref(false)

const disabledVal = computed(() => {
  const arr = []
  workshopName.value.forEach(v => {
    if (v.workshopId) {
      arr.push(v.id)
    }
  })
  list.value.forEach(v => {
    if (v.workshopId) {
      arr.push(v.workshopId)
    }
  })
  return arr
})

watch(
  visible,
  (val) => {
    list.value = []
  }
)

const tableRules = {
  name: [{ required: true, message: '请输入或选择车间', trigger: ['blur', 'change'] }],
  type: [{ required: true, message: '请选择类型', trigger: 'change' }]
}

const store = useStore()

const list = ref([])
const drawerRef = ref()

const { maxHeight } = useMaxHeight(
  {
    mainBox: '.workshop-form',
    extraBox: '.el-drawer__header',
    wrapperBox: '.el-drawer__body',
    paginate: true,
    minHeight: 300,
    navbar: false,
    clientHRepMainH: true
  },
  drawerRef
)

const { tableValidate, wrongCellMask } = useTableValidate({ rules: tableRules })

function workshopChange(val) {
  if (isNotBlank(val)) {
    list.value[val.choseIndex].name = val.name
  }
}

function addItem() {
  list.value.push({
    type: undefined,
    name: undefined
  })
}

async function onSubmit(val) {
  const { validResult, dealList } = tableValidate(list.value)
  if (validResult) {
    list.value = dealList
  } else {
    return validResult
  }
  submitLoading.value = true
  try {
    await addWorkshop(list.value)
    ElNotification({ title: '删除成功', type: 'success' })
    store.dispatch('config/fetchWorkshopName')
    handleClose()
  } catch (error) {
    console.log('审核', error)
  } finally {
    submitLoading.value = false
  }
}

function deleteItem(index) {
  list.value.splice(index, 1)
}
</script>
