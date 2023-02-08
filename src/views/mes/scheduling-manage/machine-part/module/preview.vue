<template>
  <common-dialog customClass="machine-part-scheduling-preview-dlg" title="零件排产预览" v-model="dialogVisible" width="1100px" :before-close="handleClose">
    <template #titleRight>
      <common-button @click="submitIt" :loading="submitLoading" size="mini" type="primary">保存</common-button>
    </template>
    <!-- <div class="head-container">
      <common-select
        v-model="layWayConfigId"
        :options="layingWayList"
        :loading="layingWayLoading"
        :dataStructure="{ key: 'id', label: 'layingOffWayName', value: 'id' }"
        clearable
        class="filter-item"
        placeholder="请选择下料方式"
        style="width: 200px"
      >
        <template #view="{ data }">
          <span class="customize-option-item">
            <span class="label">{{ data.layingOffWayName }}</span>
            <span>
              <span class="extra-label">
                <span v-parse-enum="{ e: materialTypeEnum, v: data.materialType }"></span>
              </span>
            </span>
          </span>
        </template>
      </common-select>
    </div> -->
    <common-table :data="list" :data-format="dataFormat" :max-height="maxHeight" style="width: 100%">
      <el-table-column label="序号" type="index" align="center" width="60" />
      <el-table-column :show-overflow-tooltip="true" prop="project" label="所属项目" min-width="120px" align="center" />
      <el-table-column :show-overflow-tooltip="true" prop="serialNumber" label="编号" min-width="80px" align="center">
        <template #default="{ row }">
          <span>{{ row.serialNumber }}</span>
        </template>
      </el-table-column>
      <el-table-column :show-overflow-tooltip="true" prop="specification" label="规格" min-width="80px" align="center">
        <template #default="{ row }">
          <span>{{ row.specification }}</span>
        </template>
      </el-table-column>
      <el-table-column :show-overflow-tooltip="true" prop="length" :label="`长度(mm)`" min-width="80px" align="center">
        <template #default="{ row }">
          <span>{{ row.length }}</span>
        </template>
      </el-table-column>
      <el-table-column :show-overflow-tooltip="true" prop="netWeight" :label="`单净重(kg)`" min-width="80px" align="center">
        <template #default="{ row }">
          <span>{{ row.netWeight }}</span>
        </template>
      </el-table-column>
      <el-table-column prop="quantity" :show-overflow-tooltip="true" label="数量" width="80" align="center" />
    </common-table>
  </common-dialog>
</template>

<script setup>
import { save } from '@/api/mes/scheduling-manage/machine-part'
import { ElNotification } from 'element-plus'
import { defineEmits, defineProps, ref, inject } from 'vue'
// import { materialTypeEnum } from '@enum-ms/uploading-form'

import useMaxHeight from '@compos/use-max-height'
import useVisible from '@compos/use-visible'

const emit = defineEmits(['update:visible', 'success'])
const props = defineProps({
  visible: {
    type: Boolean,
    default: false
  },
  list: {
    type: Array,
    default: () => []
  }
})

const dataFormat = ref([['project', 'parse-project']])

// const layWayConfigId = ref()
// const layingWayList = ref([])
// const layingWayLoading = ref(false)
const submitLoading = ref(false)
const crud = inject('crud')

const { visible: dialogVisible, handleClose } = useVisible({ emit, props, field: 'visible' })
const { maxHeight } = useMaxHeight(
  {
    mainBox: '.machine-part-scheduling-preview-dlg',
    extraBox: ['.el-dialog__header'],
    wrapperBox: ['.el-dialog__body'],
    clientHRepMainH: true,
    minHeight: 300,
    navbar: false
  },
  dialogVisible
)

// async function fetchLayingWay() {
//   try {
//     layWayConfigId.value = undefined
//     layingWayLoading.value = true
//     const content = await getLayingWay()
//     layingWayList.value = content.filter((v) => {
//       if (crud.query.thickList === '其他') {
//         return v.materialType === materialTypeEnum.MANMADE_BLANKING.V
//       } else {
//         return true
//       }
//     })
//   } catch (error) {
//     console.log('获取下料方式列表', error)
//   } finally {
//     layingWayLoading.value = false
//   }
// }

async function submitIt() {
  try {
    // if (!layWayConfigId.value) {
    //   ElMessage.warning('请选择下料方式')
    //   return
    // }
    submitLoading.value = true
    const _list = []
    props.list.forEach((v) => {
      v.needMachinePartLinkList.forEach(o => {
        _list.push(
          {
            productId: v.id,
            quantity: o.quantity,
            id: o.id,
            needSchedulingMonth: o.date
          }
        )
      })
    })
    await save({
      // layWayConfigId: layWayConfigId.value,
      material: crud.query.material,
      thickList: crud.query.thickList,
      linkList: _list
    })
    ElNotification({
      title: '零件排产保存成功',
      type: 'success',
      duration: 2500
    })
    handleClose()
    emit('success')
  } catch (error) {
    console.log('保存零件排产报错', error)
  } finally {
    submitLoading.value = false
  }
}
</script>
