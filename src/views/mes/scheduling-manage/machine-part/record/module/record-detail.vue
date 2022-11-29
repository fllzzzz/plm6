<template>
  <common-dialog title="套料明细" v-model="dialogVisible" width="1400px" :before-close="handleClose">
    <common-table v-loading="tableLoading" :data="list" :data-format="dataFormat" :max-height="maxHeight" style="width: 100%">
      <el-table-column label="序号" type="index" align="center" width="60" />
      <el-table-column :show-overflow-tooltip="true" label="所属项目>单体>区域" min-width="180px" align="left">
        <template #default="{ row }">
          <span>{{ row.project }}>{{ row.monomer.name }}>{{ row.area.name }}</span>
        </template>
      </el-table-column>
      <el-table-column :show-overflow-tooltip="true" prop="serialNumber" label="编号" min-width="80px" align="center">
        <template #default="{ row }">
          <span>{{ row.serialNumber }}</span>
        </template>
      </el-table-column>
      <el-table-column :show-overflow-tooltip="true" prop="specification" label="规格" min-width="100px" align="center">
        <template #default="{ row }">
          <span>{{ row.specification }}</span>
        </template>
      </el-table-column>
      <el-table-column :show-overflow-tooltip="true" prop="length" :label="`长度(mm)`" min-width="80px" align="center">
        <template #default="{ row }">
          <span>{{ row.length }}</span>
        </template>
      </el-table-column>
      <el-table-column :show-overflow-tooltip="true" prop="material" label="材质" min-width="80px" align="center">
        <template #default="{ row }">
          <span>{{ row.material }}</span>
        </template>
      </el-table-column>
      <el-table-column :show-overflow-tooltip="true" prop="netWeight" :label="`单净重(kg)`" min-width="80px" align="center">
        <template #default="{ row }">
          <span>{{ row.netWeight }}</span>
        </template>
      </el-table-column>
      <el-table-column prop="quantity" :show-overflow-tooltip="true" label="任务数" width="80" align="center" />
      <el-table-column :show-overflow-tooltip="true" label="图形" width="100" align="center">
        <template  #default="{ row }">
          <div style="width: 100%; height: 80px">
            <el-image style="width: 100%; height: 100%" :src="row.picturePath" fit="scale-down" />
          </div>
        </template>
      </el-table-column>
      <!-- <el-table-column label="操作" width="80" align="center">
        <template #default="{ row }">
          <el-popconfirm confirm-button-text="确定" cancel-button-text="取消" title="确定删除吗?" @confirm="rowDelete(row)">
            <template #reference>
              <common-button size="mini" type="danger">删除</common-button>
            </template>
          </el-popconfirm>
        </template>
      </el-table-column> -->
    </common-table>
  </common-dialog>
</template>

<script setup>
// import { recordDetail, delDetail } from '@/api/mes/scheduling-manage/machine-part'
import { recordDetail } from '@/api/mes/scheduling-manage/machine-part'
import { defineEmits, defineProps, ref } from 'vue'
// import { ElNotification } from 'element-plus'

import useMaxHeight from '@compos/use-max-height'
import useVisible from '@compos/use-visible'

const emit = defineEmits(['update:visible', 'del-success'])
const props = defineProps({
  visible: {
    type: Boolean,
    default: false
  },
  recordId: {
    type: Number
  }
})

const { visible: dialogVisible, handleClose } = useVisible({ emit, props, field: 'visible', showHook: fetch })
const { maxHeight } = useMaxHeight(
  {
    extraBox: ['.el-dialog__header'],
    wrapperBox: ['.el-dialog__body'],
    clientHRepMainH: true,
    minHeight: 300,
    navbar: false
  },
  dialogVisible
)

const dataFormat = ref([['project', 'parse-project']])
const list = ref([])
const tableLoading = ref(false)

async function fetch() {
  try {
    tableLoading.value = true
    const content = await recordDetail(props.recordId)
    list.value = content
  } catch (error) {
    console.log(error, '获取零件套料明细报错')
  } finally {
    tableLoading.value = false
  }
}

// async function rowDelete(row) {
//   try {
//     await delDetail([row.id])
//     ElNotification({ title: '套料明细删除成功', type: 'success', duration: 3000 })
//     fetch()
//     emit('del-success')
//   } catch (e) {
//     console.log(`删除失败`, e)
//   }
// }
</script>
