
<template>
  <div>
    <el-card :style="{ height: `${maxHeight}px` }">
      <template #header>
        <div class="card-header flex-css">
          <div class="flex-rbc">列表</div>
        </div>
      </template>
      <div class="card-body">
        <mHeader :headSelect="headSelect" />
        <!--表格渲染-->
        <common-table
          ref="tableRef"
          v-if="refreshTable"
          v-loading="crud.loading"
          :data="crud.data"
          @selection-change="handleSelectionChange"
          :empty-text="crud.emptyText"
          :max-height="maxHeight - 270"
          style="width: 100%"
        >
          <el-table-column type="selection" align="center" width="55" />
          <el-table-column prop="index" label="序号" align="center" width="60" type="index" />
          <el-table-column
            align="center"
            v-if="columns.visible('projectName')"
            key="projectName"
            prop="projectName"
            :show-overflow-tooltip="true"
            label="项目"
            min-width="70"
          >
            <template v-slot="scope">
              <span>{{ scope.row.projectName }}</span>
            </template>
          </el-table-column>
          <el-table-column
            align="center"
            v-if="columns.visible('cutInstructionId')"
            key="cutInstructionId"
            prop="cutInstructionId"
            :show-overflow-tooltip="true"
            label="切割编号id"
            min-width="70"
          >
            <template v-slot="scope">
              <span>{{ scope.row.cutInstructionId }}</span>
            </template>
          </el-table-column>
          <el-table-column
            v-if="columns.visible('machineName')"
            key="machineName"
            prop="machineName"
            :show-overflow-tooltip="true"
            label="机器名称"
            min-width="60"
          >
            <template v-slot="scope">
              <span>{{ scope.row.machineName }}</span>
            </template>
          </el-table-column>
          <el-table-column
            v-if="columns.visible('material')"
            key="material"
            prop="material"
            :show-overflow-tooltip="true"
            label="材质"
            min-width="45"
          >
            <template v-slot="scope">
              <span>{{ scope.row.material }}</span>
            </template>
          </el-table-column>
          <el-table-column
            v-if="columns.visible('thick')"
            key="thick"
            prop="thick"
            :show-overflow-tooltip="true"
            label="厚(mm)"
            min-width="45"
          >
            <template v-slot="scope">
              <span>{{ scope.row.thick }}</span>
            </template>
          </el-table-column>
          <el-table-column
            v-if="columns.visible('width')"
            key="width"
            prop="width"
            :show-overflow-tooltip="true"
            label="宽(mm)"
            min-width="45"
          >
            <template v-slot="scope">
              <span>{{ scope.row.width }}</span>
            </template>
          </el-table-column>
          <el-table-column
            v-if="columns.visible('length')"
            key="length"
            prop="length"
            :show-overflow-tooltip="true"
            label="长(mm)"
            min-width="45"
          >
            <template v-slot="scope">
              <span>{{ scope.row.length }}</span>
            </template>
          </el-table-column>
          <el-table-column
            v-if="columns.visible('plateState')"
            key="plateState"
            prop="plateState"
            :show-overflow-tooltip="true"
            label="状态"
            min-width="45"
          >
            <template v-slot="scope">
              <span>{{ steelPlateEnum.VL[scope.row.plateState] }}</span>
            </template>
          </el-table-column>
          <el-table-column
            v-if="columns.visible('uploadName')"
            key="uploadName"
            prop="uploadName"
            :show-overflow-tooltip="true"
            label="上传人"
            min-width="60"
          >
            <template v-slot="scope">
              <span>{{ scope.row.uploadName }}</span>
            </template>
          </el-table-column>
          <el-table-column min-width="110" :show-overflow-tooltip="true" label="操作" align="center">
            <template v-slot="scope">
              <common-button size="mini" type="primary" @click="viewDetails(scope.row)">查 看</common-button>
              <common-button v-if="scope.row.plateState === '1'" size="mini" type="success" @click="Issue(scope.row)">下 发</common-button>
            </template>
          </el-table-column>
        </common-table>
        <!--分页组件-->
        <pagination />
        <detail :detail-data="detailObj" v-model:visible="specsVisible" />
      </div>
    </el-card>
  </div>
</template>

<script setup>
import crudApi from '@/api/cutting/project-data'
import { ref, defineEmits, defineProps, watch, nextTick } from 'vue'
import mHeader from './plant-header'
import useCRUD from '@compos/use-crud'
import pagination from '@crud/Pagination'
import useMaxHeight from '@compos/use-max-height'
import { steelPlateEnum } from '@enum-ms/cutting'
import detail from '../../template/detail.vue'
import { sentTask } from '@/api/cutting/machine'
import { ElMessage } from 'element-plus'

const props = defineProps({
  BooleanValue: { type: Boolean, required: true }
})

// 刷新表格
const refreshTable = ref(true)
const headSelect = ref([])

watch(
  () => props.BooleanValue,
  (value, oldValue) => {
    if (value !== oldValue) {
      nextTick(() => {
        crud.toQuery()
      })
    }
  },
  { immediate: true }
)

// crud交由presenter持有
const permission = { get: ['contractRecord:get'] }
const emit = defineEmits(['selectionChange'])

const tableRef = ref()
const detailObj = ref([])
const specsVisible = ref(false)

const optShow = {
  add: false,
  edit: false,
  del: true,
  download: false
}

const { crud, columns } = useCRUD(
  {
    title: '项目数据',
    sort: [],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi },
    hasPagination: true
  },
  tableRef
)

const { maxHeight } = useMaxHeight({
  wrapperBox: '.contractRecord',
  paginate: true,
  extraHeight: 40
})

// 查看详情
function viewDetails(row) {
  specsVisible.value = true
  detailObj.value = row
}

async function Issue(row) {
  try {
    const data = []
    data.push(row.id)
    await sentTask(data)
    ElMessage({ message: '下发成功！！！', type: 'success' })
    crud.toQuery()
  } catch (err) {
    console.log(err)
  }
}

function handleSelectionChange(val) {
  headSelect.value = val.map(item => { return item.id })
  const data = []
  val.forEach(arr => {
    console.log('arr', arr)
    if (arr.plateState === '0') {
      data.push(arr)
    }
  })
  emit('selectionChange', data)
}

</script>

<style lang="scss" scoped>
::v-deep(.el-table) {
  margin-top: 10px;
}
</style>
