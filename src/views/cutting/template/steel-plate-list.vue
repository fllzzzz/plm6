<template>
  <common-dialog
    title="钢板清单"
    width="70%"
    :show-close="false"
    :close-on-click-modal="false"
    v-model="drawerVisible"
    :before-close="handleClose"
  >
    <template #titleRight>
      <common-button size="mini" @click="handleClose">关 闭</common-button>
    </template>
    <div class="flex-rss">
      <common-table v-loading="tabLoading" row-key="id" ref="tableRef" :max-height="500" style="width: 100%" :data="plateData">
        <el-table-column label="序号" type="index" align="center" width="60" />
        <el-table-column key="cutInstructionId" prop="cutInstructionId" :show-overflow-tooltip="true" label="指令号" min-width="100">
          <template v-slot="scope">
            <span>{{ scope.row.cutInstructionId }}</span>
          </template>
        </el-table-column>
        <el-table-column align="center" key="num" prop="num" :show-overflow-tooltip="true" label="数量" min-width="35">
          <!-- <template v-slot="scope">
            <span>{{ scope.row.num }}</span>
          </template> -->
          <span>1</span>
        </el-table-column>
        <el-table-column key="material" align="center" prop="material" :show-overflow-tooltip="true" label="材质" min-width="40">
          <template v-slot="scope">
            <span>{{ scope.row.material }}</span>
          </template>
        </el-table-column>
        <el-table-column align="center" key="thick" prop="thick" :show-overflow-tooltip="true" label="厚度（mm）" min-width="50">
          <template v-slot="scope">
            <span>{{ scope.row.thick }}</span>
          </template>
        </el-table-column>
        <el-table-column key="width" prop="width" :show-overflow-tooltip="true" label="宽度（mm）" min-width="55">
          <template v-slot="scope">
            <span>{{ scope.row.width }}</span>
          </template>
        </el-table-column>
        <el-table-column key="length" prop="length" :show-overflow-tooltip="true" label="长度（mm）" min-width="55">
          <template v-slot="scope">
            <span>{{ scope.row.length }}</span>
          </template>
        </el-table-column>

        <el-table-column key="weight" prop="weight" :show-overflow-tooltip="true" label="单重（kg）" min-width="55">
          <template v-slot="scope">
            <span>{{ scope.row.weight }}</span>
          </template>
        </el-table-column>
        <el-table-column key="weight" prop="weight" :show-overflow-tooltip="true" label="总重（kg）" min-width="55">
          <template v-slot="scope">
            <span>{{ scope.row.weight }}</span>
          </template>
        </el-table-column>
        <el-table-column align="center" :show-overflow-tooltip="true" label="套料成果" min-width="70">
          <template v-slot="scope">
            <common-button type="success" size="mini" @click="nestResults(scope.row)">查看</common-button>
          </template>
        </el-table-column>
        <el-table-column align="center" :show-overflow-tooltip="true" label="操作" min-width="70">
          <template v-slot="scope">
            <common-button type="danger" size="mini" @click="del(scope.row)">删除</common-button>
          </template>
        </el-table-column>
      </common-table>
    </div>
    <el-pagination
      v-model:page-size="page.size"
      v-model:current-page="page.page"
      :total="page.total"
      style="margin-top: 8px"
      layout="total, prev, pager, next, sizes"
      @size-change="sizeChangeHandler($event)"
      @current-change="pageChangeHandler"
    />
  </common-dialog>

  <detail @change="handleChange" :detail-data="detailObj" v-model:visible="specsVisible" />
</template>

<script setup>
import useVisible from '@compos/use-visible'
import detail from '@/views/cutting/template/detail.vue'

import { defineProps, defineEmits, ref } from 'vue'
import { get } from '@/api/cutting/project-data'

const props = defineProps({
  visible: {
    type: Boolean,
    required: true
  },
  detailData: {
    type: Object
  }
})

const page = {
  size: 20,
  page: 1,
  total: null
}
const tabLoading = ref(false)
const emit = defineEmits(['update:visible'])
const { visible: drawerVisible, handleClose } = useVisible({ emit, props, field: 'visible', closeHook, showHook })

const plateData = ref([]) // 页面数据
const specsVisible = ref(false)
const selectLineId = ref()
const detailObj = ref([])

function showHook() {
  if (props.detailData) {
    plateDataGet()
  }
}

function closeHook() {
  page.size = 20
  page.page = 1
}

async function plateDataGet() {
  tabLoading.value = true
  try {
    const data = await get({ projectId: props.detailData.projectId, pageSize: page.size, pageNumber: page.page })
    page.total = data.totalElements
    plateData.value = data.content
  } catch (err) {
    console.log('钢板清单页面接口报错', err)
  }
  tabLoading.value = false
}

function nestResults(row) {
  detailObj.value = row
  specsVisible.value = true
}

function del(row) {
  console.log(row)
}

function handleChange(row) {
  selectLineId.value = row.id
}

function sizeChangeHandler(e) {
  page.size = e
  plateDataGet()
}

function pageChangeHandler(a) {
  page.page = a
  plateDataGet()
}
</script>

<style rel="stylesheet/scss" lang="scss" scoped>
.item-name {
  padding: 8px 16px;
  background-color: #ecf8ff;
  border-radius: 4px;
  border-left: 5px solid rgb(81, 113, 131);
  margin: 10px 0;
  margin-left: 5px;
  width: 150px;
}
.TaskPackage {
  margin-top: 30px;
}
.title-style {
  font-weight: 700;
  font-size: 18px;
  color: #000;
}
</style>

