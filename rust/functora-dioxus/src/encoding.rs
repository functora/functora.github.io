// re-export of the shared pure logic crate
pub use functora_core::encoding::*;

#[must_use]
pub fn download_script(filename: &str) -> String {
    let escaped = filename
        .replace('\\', "\\\\")
        .replace('"', "\\\"")
        .replace('\n', "\\n")
        .replace('\r', "\\r")
        .replace('\t', "\\t")
        .replace('\'', "\\u0027")
        .replace('<', "\\u003c")
        .replace('>', "\\u003e");
    format!(
        r#"const parts=[];for(;;){{const m=await dioxus.recv();if(m&&m.t==='done')break;if(m&&m.t==='abort')return;const bin=atob(m.data);const bytes=new Uint8Array(bin.length);for(let i=0;i<bin.length;i++)bytes[i]=bin.charCodeAt(i);parts.push(bytes)}}const url=URL.createObjectURL(new Blob(parts,{{type:'application/octet-stream'}}));const a=document.createElement('a');a.href=url;a.download="{escaped}";a.style.display='none';document.body.appendChild(a);a.click();setTimeout(()=>{{document.body.removeChild(a);URL.revokeObjectURL(url)}},1000)"#,
    )
}
