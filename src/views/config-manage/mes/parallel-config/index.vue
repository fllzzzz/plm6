<template>
  <div class="app-container">
    <!--工具栏-->
    <div class="head-container">
      <mHeader />
    </div>
    <!--表格渲染-->
    <common-table
      ref="tableRef"
      class="upload-table"
      v-loading="crud.loading"
      :data="crud.data"
      :empty-text="crud.emptyText"
      :max-height="maxHeight"
      return-source-data
      :showEmptySymbol="false"
      :stripe="false"
      style="width: 100%"
    >
      <el-table-column prop="index" label="序号" align="center" width="60" type="index" />
      <!-- <el-table-column
        v-if="columns.visible('name')"
        key="name"
        prop="name"
        sortable="custom"
        :show-overflow-tooltip="true"
        label="名称"
        min-width="100px"
      /> -->
      <el-table-column
        v-if="columns.visible('serialNumber')"
        key="serialNumber"
        prop="serialNumber"
        sortable="custom"
        :show-overflow-tooltip="true"
        label="编号"
        min-width="140px"
      />
      <el-table-column
        v-if="columns.visible('specification')"
        key="specification"
        prop="specification"
        sortable="custom"
        :show-overflow-tooltip="true"
        label="规格"
        min-width="120"
      >
        <template v-slot="scope">
          {{ scope.row.specification ? scope.row.specification : '-' }}
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('length')"
        key="length"
        prop="length"
        sortable="custom"
        :show-overflow-tooltip="true"
        :label="`长度\n(mm)`"
        align="left"
        min-width="85px"
      >
        <template v-slot="scope">
          {{ scope.row.length ? scope.row.length.toFixed(DP.MES_ARTIFACT_L__MM) : '-' }}
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('thick')"
        key="thick"
        prop="thick"
        sortable="custom"
        :show-overflow-tooltip="true"
        :label="`厚度\n(mm)`"
        align="left"
        min-width="85px"
      >
        <template v-slot="scope">
          {{ scope.row.thick ? scope.row.thick.toFixed(DP.MES_ARTIFACT_T__MM) : '-' }}
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('material')"
        key="material"
        prop="material"
        sortable="custom"
        :show-overflow-tooltip="true"
        label="材质"
        align="center"
        min-width="80px"
      >
        <template v-slot="scope">
          {{ scope.row.material ? scope.row.material : '-' }}
        </template>
      </el-table-column>
      <!-- <el-table-column
        v-if="columns.visible('quantity')"
        key="quantity"
        prop="quantity"
        sortable="custom"
        label="数量"
        align="left"
        min-width="80px"
      >
        <template v-slot="scope">
          {{ scope.row.quantity ? scope.row.quantity : '-' }}
        </template>
      </el-table-column> -->
      <el-table-column
        v-if="columns.visible('netWeight')"
        key="netWeight"
        prop="netWeight"
        sortable="custom"
        :show-overflow-tooltip="true"
        :label="`单净重\n(kg)`"
        align="left"
        min-width="80px"
      >
        <template v-slot="scope">
          {{ scope.row.netWeight ? scope.row.netWeight.toFixed(DP.COM_WT__KG) : '-' }}
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('grossWeight')"
        key="grossWeight"
        prop="grossWeight"
        sortable="custom"
        :show-overflow-tooltip="true"
        :label="`单毛重\n(kg)`"
        align="left"
        min-width="80px"
      >
        <template v-slot="scope">
          {{ scope.row.grossWeight ? scope.row.grossWeight.toFixed(DP.COM_WT__KG) : '-' }}
        </template>
      </el-table-column>
      <!-- <el-table-column
        v-if="columns.visible('totalNetWeight')"
        key="totalNetWeight"
        prop="totalNetWeight"
        :show-overflow-tooltip="true"
        :label="`总净重\n(kg)`"
        align="left"
        min-width="95px"
      >
        <template v-slot="scope">
          {{ scope.row.totalNetWeight ? scope.row.totalNetWeight.toFixed(DP.COM_WT__KG) : '-' }}
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('totalGrossWeight')"
        key="totalGrossWeight"
        prop="totalGrossWeight"
        :show-overflow-tooltip="true"
        :label="`总毛重\n(kg)`"
        align="left"
        min-width="95px"
      >
        <template v-slot="scope">
          {{ scope.row.totalGrossWeight ? scope.row.totalGrossWeight.toFixed(DP.COM_WT__KG) : '-' }}
        </template>
      </el-table-column> -->
      <el-table-column
        v-if="columns.visible('surfaceArea')"
        key="surfaceArea"
        prop="surfaceArea"
        sortable="custom"
        :label="`单面积\n(㎡)`"
        align="left"
        min-width="80px"
      >
        <template v-slot="scope">
          {{ scope.row.surfaceArea ? scope.row.surfaceArea.toFixed(DP.COM_AREA__M2) : '-' }}
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('boolUpload')" align="center" prop="boolUpload" label="dxf上传">
        <template v-slot="scope">
          <upload-btn
            v-permission="permission.import"
            :accept="`.dxf`"
            :data="{id:scope.row.id,dataType:2}"
            :tip="uploadType"
            size="mini"
            :uploadFun="uploadFun"
            :btn-name="scope.row.boolUpload?'覆盖上传':'上传'"
            type="primary"
            :limit="1"
            @success="handleSuccess"
          />
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('picturePath')"
        key="picturePath"
        prop="picturePath"
        label="图片"
        align="left"
        min-width="80px"
      >
        <template v-slot="scope">
          <div class="board-box">
            <el-image :src="scope.row.picturePath" @error="item.imgLoad = false">
              <template #error>
                <div class="error-slot">
                  <span v-if="scope.row.picturePath">加载失败</span>
                  <span v-else>未导入DXF</span>
                </div>
              </template>
            </el-image>
          </div>
        </template>
      </el-table-column>
      <!--编辑与删除-->
      <el-table-column
        v-if="checkPermission([...permission.del, ...permission.edit])"
        label="操作"
        width="100px"
        align="center"
        fixed="right"
      >
        <template v-slot="scope">
          <ud-operation :data="scope.row" :show-edit="false"/>
        </template>
      </el-table-column>
    </common-table>
    <!--分页组件-->
    <pagination />
    <mForm />
  </div>
</template>

<script setup>
import crudApi, { uploadFun } from '@/api/config/system-config/parallel-config'
import { ref } from 'vue'

import { mesParallelConfigPM as permission } from '@/page-permission/config'
import { ElMessage } from 'element-plus'

import { DP } from '@/settings/config'
import checkPermission from '@/utils/system/check-permission'
import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import pagination from '@crud/Pagination'
import udOperation from '@crud/UD.operation'
import mHeader from './module/header'
import mForm from './module/form'
import UploadBtn from '@comp/file-upload/SingleFileUploadBtn'

const optShow = {
  add: true,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const { CRUD, crud, columns } = useCRUD(
  {
    title: '垫片配置',
    sort: [],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi },
    hasPagination: true
  },
  tableRef
)

const { maxHeight } = useMaxHeight({
  wrapperBox: '.parallelConfig',
  paginate: true,
  extraHeight: 40
})

function handleSuccess() {
  ElMessage.success('上传成功')
  crud.toQuery()
}

CRUD.HOOK.handleRefresh = (crud, res) => {
  res.data.content = res.data.content.map((v) => {
    v.imgLoad = true
    return v
  })
}
</script>
<style lang="scss" scoped>
 .board-box {
    width: 80px;
    height: 80px;
    line-height:80px;
    text-align:center;
    box-sizing: border-box;
    padding: 2px;
    border: 1px solid #dfe4ed;
    border-radius: 6px;
  }
</style>
